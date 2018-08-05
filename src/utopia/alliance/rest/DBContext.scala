package utopia.alliance.rest

import utopia.vault.database.Connection
import utopia.nexus.http.ServerSettings
import utopia.nexus.rest.Context
import utopia.nexus.result.ResultParser
import utopia.nexus.result.UseRawJSON
import utopia.nexus.http.Request
import utopia.vault.model.Table
import utopia.flow.datastructure.immutable.Constant
import utopia.flow.datastructure.immutable.Model
import utopia.vault.model.DBModel

/**
* This context offers a database connection for its clients
* @author Mikko Hilpinen
* @since 22.5.2018
**/
class DBContext(val request: Request, val resultParser: ResultParser = UseRawJSON)
        (implicit val settings: ServerSettings) extends Context
{
    // ATTRIBUTES    -------------------
    
    /**
     * The database connection used in this context
     */
    val connection: Connection = new Connection()
    
    
    // IMPLEMENTED    -----------------
    
	override def close() = connection.close()
	
	
	// OTHER    -----------------------
	
	// TODO: Add support for additional parameter handlers
	/**
	 * Finds query parameters that concern a specific table
	 * @param table the target table
	 * @param includeAutoIncrement whether values for auto-increment columns should be included
	 */
	def parametersForTable(table: Table, includeAutoIncrement: Boolean) = 
	{
        // Includes
        // a) Parameters that start with the table name (parsed)
        // b) Parameters that match a varname from the table
        val paramVars = request.parameters.attributes
        val tablePrefix = table.name + "."
        val prefixVars = paramVars.flatMap(c => if (c.name.startsWith(tablePrefix)) 
                Some(new Constant(c.name.substring(tablePrefix.length()), c.value)) else None);
        
        new Model((paramVars ++ prefixVars).filter(c => table.find(c.name).exists(
                includeAutoIncrement || !_.usesAutoIncrement)))
	}
    
    // def rawParameters = 
}