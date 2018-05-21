package utopia.alliance.model

import utopia.vault.model.Table
import utopia.vault.model.Column
import utopia.vault.model.Reference

object Bridge
{
    /**
     * A bridge between the two references. None if the references aren't linked
     */
    def between(first: Reference, second: Reference) = if (first.to.table == second.from.table) 
            Some(Bridge(first.to.table, first.to.column, second.from.column)) else None
}

/**
* A bridge is a table that binds two tables together via references in order to create a 
* many to many relation
* @author Mikko Hilpinen
* @since 21.5.2018
**/
case class Bridge(val table: Table, val leftColumn: Column, val rightColumn: Column)
{
    /**
     * This bridge to the opposite direction
     */
	def reversed = Bridge(table, rightColumn, leftColumn)
}