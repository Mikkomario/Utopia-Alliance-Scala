package utopia.alliance.rest

import utopia.vault.database.Connection
import utopia.nexus.http.ServerSettings
import utopia.nexus.rest.Context
import utopia.nexus.result.ResultParser
import utopia.nexus.result.UseRawJSON
import utopia.nexus.http.Request

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
	
	
}