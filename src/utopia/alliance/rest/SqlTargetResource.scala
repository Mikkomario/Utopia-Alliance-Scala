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
import utopia.vault.sql.SqlSegment
import utopia.vault.sql.Update
import scala.collection.immutable.HashMap
import utopia.vault.sql.Delete

/**
* This class wraps a non-read sql target as a resource that can be followed or instantiated
* @author Mikko Hilpinen
* @since 5.8.2018
**/
class SqlTargetResource(val parent: TableResource, val name: String, val target: SqlTarget, 
        val condition: Option[Condition] = None) extends Resource[DBContext]
{
    // TODO: Add parameter handlers (to tableResource) (eg. basic parameter conditions, where, custom)
    
    // COMPUTED    -------------------------
    
    /**
     * The primary table used by this resource
     */
    def table = parent.table
    
    
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
	            new SqlTargetResource(resource, refName, relation.toSqlTarget(target), condition)
	        }
	    }
	}
	
	// Adds possible where clause to query
	private def statementWithCondition(baseStatement: SqlSegment) = condition.map(
	        baseStatement + Where(_)).getOrElse(baseStatement);
	
	// Reads model data
	private def read()(implicit connection: Connection) = Try(connection(
	        statementWithCondition(SelectAll(target))).rowModels);
	
	// Reads model data and wraps it as a resource, if possible
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
	            Some(new RowResource(DBModel(table, models.head), parent))
	        }
	        else
	        {
	            Some(new ListResource(name, parent, models.map(DBModel(table, _))))
	        }
	    })
	}
	
	// Updates DB data based on request parameters
	private def update()(implicit context: DBContext) = 
	{
	    // Updates parameter values to target
	    val baseUpdate = Update(target, HashMap(table -> context.parametersForTable(table, false)))
	    // Also reads the new status and returns it
	    Try(baseUpdate.map(statementWithCondition).foreach(context.connection.apply)).flatMap(
	            u => read()(context.connection));
	}
	
	// Deletes items from the primary table
	private def delete()(implicit connection: Connection) = Try(connection(
	        statementWithCondition(Delete(target, Vector(table)))));
}