package utopia.alliance.model

import utopia.vault.model.Reference
import utopia.vault.model.Table
import utopia.vault.model.References
import utopia.vault.sql.Join
import utopia.vault.model.ReferencePoint

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
                _.isRequiredInInsert);
        
        bridges.flatMap(_.requiredPostParams) ++ requiredByTarget
    }
}