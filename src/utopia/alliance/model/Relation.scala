package utopia.alliance.model

import utopia.vault.model.Reference
import utopia.vault.model.Table
import utopia.vault.model.References
import utopia.vault.sql.Join

object Relation
{
    /**
     * a relation that bridges the provided tables. None if there is no relation between the tables.<br>
     * References must be set up before this method can be used.
     */
    def apply(relationType: RelationType, first: Table, second: Table, more: Table*) = 
    {
        referencesBridging(first +: second +: more).map 
        {
            references => 
            val bridges = for (i <- 0 until (references.size - 1)) yield { 
                    Bridge.between(references(i), references(i + 1)).get }
            
            new Relation(relationType, Reference(references.head.from, references.last.to), bridges)
        }
    }
    
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
        Relation(relationType, first, second, more: _*)
    }
    
    /**
     * a relation that bridges the provided tables. None if there is no relation between 
     * the tables. Guesses the relation type from the table references.<br>
     * References must be set up before this method can be used
     */
    def between(first: Table, second: Table, more: Table*) = 
    {
        val tables = first +: second +: more
        val style: Option[RelationType] = referencesBridging(tables).map
        {
            references => 
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
        
        style.flatMap(forTables(_, first, second, more))
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
case class Relation(val relationType: RelationType, val reference: Reference, 
        val bridges: Seq[Bridge] = Vector())
{
    /**
     * The table the relation starts from
     */
    def from = reference.from.table
    
    /**
     * The table the relation points to
     */
    def to = reference.to.table
    
    /**
     * All references that form this relation
     */
    def references = 
    {
        if (bridges.isEmpty)
            Vector(reference)
        else
        {
            val refs = bridges.tail.foldLeft(Vector(Reference(reference.from, bridges.head.left)))(
                    (refs, bridge) => refs :+ Reference(refs.last.to, bridge.left));
            
            refs :+ Reference(bridges.last.right, reference.to)
        }
    }
    
    /**
     * Converts this relation to an sql target that contains all of the related tables
     */
    def toSqlTarget = 
    {
        val refs = references
        refs.tail.foldLeft(refs.head.toSqlTarget)((target, ref) => target + Join(ref.from.column, ref.to))
    }
}