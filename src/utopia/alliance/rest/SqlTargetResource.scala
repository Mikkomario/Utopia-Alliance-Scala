package utopia.alliance.rest

import utopia.vault.sql.SqlTarget
import utopia.vault.sql.Condition
import utopia.nexus.rest.Resource
import utopia.nexus.http.Path
import utopia.nexus.rest.ResourceSearchResult
import utopia.nexus.http.Response
import utopia.vault.sql.SelectAll
import utopia.vault.sql.Where
import utopia.vault.database.Connection
import scala.util.Try
import utopia.vault.model.DBModel

/**
* This class wraps a non-read sql target as a resource that can be followed or instantiated
* @author Mikko Hilpinen
* @since 5.8.2018
**/
class SqlTargetResource(val parent: TableResource, val name: String, val target: SqlTarget, 
        val condition: Option[Condition] = None) extends Resource[DBContext]
{
    // TODO: Add parameter handlers (to tableResource) (eg. basic parameter conditions, where, custom)
    
    // IMPLEMENTED    ----------------------
    
	def allowedMethods = parent.allowedMethods
	
	def follow(path: Path)(implicit context: DBContext): ResourceSearchResult = ???
	def toResponse(remainingPath: Option[Path])(implicit context: DBContext): Response = ???
	
	
	// OTHER    ---------------------------
	
	private def followReference(refName: String) = 
	{
	    parent.relatedResource(refName).map
	    {
	        case (relation, resource) =>
	        {
	            // TODO: Handle conditions
	            new SqlTargetResource(resource, refName, relation.toSqlTarget(target))
	        }
	    }
	}
	
	private def read()(implicit connection: Connection) = 
	{
	    // Reads the model(s) from the database
	    val baseSelect = SelectAll(target)
	    val query = condition.map(baseSelect + Where(_)).getOrElse(baseSelect)
	    
	    Try(connection(query).rowModels)
	}
	
	private def followReadResult()(implicit connection: Connection) = 
	{
	    // Maps the read models to either None (nothing found), Model (1 result) or List (multiple models)
	    read().map(models => 
	    {
	        if (models.isEmpty)
	        {
	            None
	        }
	        else if (models.size == 1)
	        {
	            Some(new RowResource(DBModel(parent.table, models.head), parent))
	        }
	        else
	        {
	            Some(new ListResource(name, parent, models.map(DBModel(parent.table, _))))
	        }
	    })
	}
	
	// TODO: Add update and delete methods once query parameters an be parsed to table values
}