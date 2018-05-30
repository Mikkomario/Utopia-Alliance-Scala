package utopia.alliance.model

import utopia.vault.model.Table
import utopia.vault.model.Column
import utopia.vault.model.Reference
import utopia.vault.model.ReferencePoint

/**
* A bridge is a table that binds two tables together via references in order to create a 
* many to many relation
* @author Mikko Hilpinen
* @since 21.5.2018
**/
case class Bridge(val left: Reference, val right: Reference)
{
    /**
     * The tables that are part of this bridge
     */
    val tables = (left.tables ++ right.tables).distinct
    
    /**
     * The table the bridging resource is located at
     */
    val table = left.tables.find(right.tables.contains).get
    
    /**
     * This bridge to the opposite direction
     */
	def reversed = Bridge(right, left)
	
	/**
	 * The left side reference point
	 */
	def leftPoint = if (left.from.table == table) left.from else left.to
	
	/**
	 * The right side reference point
	 */
	def rightPoint = if (right.from.table == table) right.from else right.to
	
	/**
	 * The parameters that must be provided if one is to insert an instance of this bridge to a 
	 * database
	 */
	def requiredPostParams = 
    {
	    val refColumns = left.columns ++ right.columns
	    table.columns.filterNot(refColumns.contains).filter(_.isRequiredInInsert).map(_.name)
    }
}