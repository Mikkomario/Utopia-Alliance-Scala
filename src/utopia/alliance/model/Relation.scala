package utopia.alliance.model

import utopia.vault.model.Reference
import utopia.vault.model.Table
import utopia.vault.model.References
import utopia.vault.sql.Join
import utopia.vault.model.ReferencePoint
import utopia.flow.datastructure.immutable.Model
import utopia.flow.datastructure.immutable.Constant
import utopia.alliance.rest.DBContext
import utopia.vault.model.DBModel
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.collection.immutable.HashMap

object Relation
{
    /**
     * a relation that bridges the provided tables. None if there is no relation between the tables.<br>
     * References must be set up before this method can be used.
     */
    def apply(relationType: RelationType, first: Table, second: Table, more: Table*) = 
            forTables(relationType, first, second, more);
    
    /**
     * A one to many relation that bridges the provided tables. None if there is no relation between 
     * the tables.<br>
     * References must be set up before this method can be used
     */
    def oneToMany(first: Table, second: Table, more: Table*) = forTables(ToMany, first, second, more)
    
    /**
     * A one to one relation that bridges the provided tables. None if there is no relation between 
     * the tables.<br>
     * References must be set up before this method can be used
     */
    def oneToOne(first: Table, second: Table, more: Table*) = forTables(ToOne, first, second, more)
    
    /**
     * A one to some relation that bridges the provided tables. None if there is no relation between 
     * the tables.<br>
     * References must be set up before this method can be used
     */
    def oneToSome(first: Table, second: Table, more: Table*) = forTables(ToSome, first, second, more)
    
    private def forTables(relationType: RelationType, first: Table, second: Table, more: Seq[Table]) = 
    {
        val allTables = first +: second +: more
        referencesBridging(allTables).map(new Relation(first, allTables.last, relationType, _))
    }
    
    /**
     * a relation that bridges the provided tables. None if there is no relation between 
     * the tables. Guesses the relation type from the table references.<br>
     * References must be set up before this method can be used
     */
    def between(first: Table, second: Table, more: Table*) = 
    {
        val tables = first +: second +: more
        referencesBridging(tables).map
        {
            references => 
                
                val style: RelationType = 
                {
                    // If the references are always for the first / left table
                    // a) if all are not null colums -> toOne relation
                    // b) if null allowed anywhere -> toSome relation
                    // Otherwise toMany relation
                    if (tables.zip(references).forall { case (table, ref) => ref.from.table == table })
                    {
                        if (references.forall(!_.from.column.allowsNull))
                            ToOne
                        else
                            ToSome
                    }
                    else
                        ToMany
                }
                
                Relation(first, tables.last, style, references)
        }
    }
    
    private def referencesBridging(tables: Seq[Table]) = 
    {
        val read = for (i <- 0 until (tables.size - 1)) yield { 
            References.between(tables(i), tables(i + 1)).headOption }
        
        if (read.forall(_.isDefined))
            Some(read.flatten)
        else
            None
    }
}

/**
* Relations are placed between different models to create object hierarchies and link systems
* @author Mikko Hilpinen
* @since 21.5.2018
**/
case class Relation(val from: Table, val to: Table, val relationType: RelationType, 
        val references: Seq[Reference])
{
    // ATTRIBUTES    ------------------
    
    /**
     * The bridges that are part of this relation (if this relation uses bridging tables)
     */
    val bridges = for (i <- 0 until (references.size - 1)) yield { Bridge(references(i), references(i + 1)) }
    
    private val links = 
    {
        var lastTable = from
        val points = references.flatMap
        {
            ref => 
                if (ref.from.table == lastTable)
                {
                    lastTable = ref.to.table
                    Vector(ref.from, ref.to)
                }
                else
                {
                    lastTable = ref.from.table
                    Vector(ref.to, ref.from)
                }
        }
        for (i <- 0 until (points.size - 1)) yield { Reference(points(i), points(i + 1)) }
    }
    
    
    // COMPUTED    --------------------
    
    /**
     * Converts this relation to an sql target that contains all of the related tables
     */
    def toSqlTarget = 
    {
        links.tail.foldLeft(links.head.toSqlTarget)((target, ref) => target + Join(ref.from.column, ref.to))
    }
    
    /**
     * All of the additional parameters that are required for a post to the target table when 
     * source table index is known
     */
    def requiredPostParams = 
    {
        val lastReferenceColums = references.last.columns
        val requiredByTarget = to.columns.filterNot(lastReferenceColums.contains).filter(
                _.isRequiredInInsert).map(_.name);
        
        bridges.flatMap(_.requiredPostParams) ++ requiredByTarget
    }
    
    def postFrom(model: Model[Constant])(implicit context: DBContext) = 
    {
        // TODO: Result should allow header modifications (for location)
        // Also, return a Result here
        // Checks if all required parameters have been provided
        if (requiredPostParams.forall(context.request.parameters.contains))
        {
            makePost(bridges.map(_.table) :+ to, HashMap(from -> model)).map(_(to))
        }
        
    }
    
    private def makePost(remainingTables: Seq[Table], existingData: Map[Table, Model[Constant]])
            (implicit context: DBContext): Try[Map[Table, Model[Constant]]] = 
    {
        // Finds the next table that can be posted
        val (table, params) = remainingTables.view.map(
                table => table -> canPost(table, existingData)).find(
                _._2.isDefined).map(p => p._1 -> p._2.get).get;
        
        val attributes = context.request.parameters.filter(p => table.find(p.name).exists(
                !_.usesAutoIncrement)) ++ params;
        val model = DBModel(table, attributes)
        
        Try(model.insert()(context.connection)) match 
        {
            case Success(index) => 
            {
                // Uses recursion to generate the remaining data
                model.index = index
                val nextRemainingTables = remainingTables.filterNot(_ == table)
                val nextExistingData = existingData + (table -> model.immutableCopy())
                
                // When no data remains, returns success
                if (nextRemainingTables.isEmpty)
                    Success(nextExistingData)
                else
                    makePost(nextRemainingTables, nextExistingData)
            }
            case Failure(e) => Failure(e)
        }
    }
    
    // Returns some(params) if OK, none if NOT OK
    private def canPost(table: Table, existingData: Map[Table, Model[Constant]]) = 
    {
        // Checks the references, if there are references from the table, the targets must already 
        // be defined
        val fromReferences = references.filter(_.from.table == table)
        if (fromReferences.forall(ref => existingData.contains(ref.to.table)))
        {
            Some(fromReferences.map(ref => new Constant(ref.from.column.name, 
                    existingData(ref.to.table)(ref.to.column.name))))
        }
        else
            None
    }
}