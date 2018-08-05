package utopia.alliance.rest

import utopia.flow.generic.ValueConversions._

import utopia.access.http.Method._

import utopia.vault.model.Storable
import utopia.vault.model.StorableFactory
import utopia.nexus.rest.Resource
import utopia.access.http.Method
import utopia.nexus.http.Path
import utopia.nexus.http.Request
import utopia.nexus.http.ServerSettings
import utopia.nexus.rest.ResourceSearchResult
import utopia.nexus.http.Response
import utopia.alliance.model.Relation
import scala.collection.immutable.HashMap
import utopia.vault.sql.Join
import utopia.vault.sql.Condition
import utopia.vault.model.Readable
import utopia.flow.datastructure.immutable.Value
import utopia.vault.model.Table
import utopia.vault.model.DBModel
import scala.util.Try
import scala.util.Success
import utopia.access.http.Created
import utopia.nexus.result.Result
import scala.util.Failure
import utopia.access.http.BadRequest
import utopia.flow.datastructure.immutable.Model
import utopia.vault.model.Reference
import utopia.nexus.rest.Ready

/**
* This resource handles data in a single table, referencing other resources when necessary
* @author Mikko Hilpinen
* @since 22.5.2018
**/
class TableResource(val table: Table, val path: Path, val relations: Map[String, Relation] = HashMap(), 
        val allowedMethods: Traversable[Method] = Vector(Get)) extends Resource[DBContext]
{
    // COMPUTED    --------------------
    
    def name = path.lastElement
	def follow(path: Path)(implicit context: DBContext): ResourceSearchResult = 
	{
        // Some functions behave differently for the last element and the rest
        val nextIsLast = path.length == 1
        val method = context.request.method
        
        // For Post, Put and Delete, handles the last part in this resource
        if (nextIsLast && method != Get)
        {
            Ready(Some(path))
        }
        else
        {
            // If the path references another resource, redirects the request to that resource
            // (except if handling the last part)
            /* TODO: This is not how related resources work. Relation is always for the index
             * -> Needs to first check if request starts with index
            relatedResource(path.head) match 
            {
                case Some((relation, target)) => 
                {
                    // Get -> Last: Redirect to row (one) or list (many), not last: redirect to target
                    // Post -> Last: 
                }
            }*/
            
            /*
            val targetResource = relatedResource(path.head)
            if (targetResource.isDefined)
            {
                
            }*/
            
            ???
        }
    }
	def toResponse(remainingPath: Option[Path])(implicit context: DBContext): Response = ???
	
	// TODO: On post handling, also create the appropriate bridges (added to Relation.postFrom)
	// TODO: Handle ordering (including default order)
	// TODO: Create read-only versions for these resources
	
	/**
	 * @return a resource for the specified relation name. Also includes the relation itself
	 */
	def relatedResource(relationName: String) = relations.get(relationName).flatMap(relation => 
	        TableResources.resourceForTable(relation.to).map(relation -> _));
	
	/**
	 * Posts a new item to this table based on the provided context
	 */
	def post()(implicit context: DBContext) = 
	{
	    // Will only include table variables, excluding any auto-increment items
	    val atts = context.request.parameters.filter(
	            c => table.find(c.name).exists(!_.usesAutoIncrement));
	    
	    // Checks that the provided attributes form a complete insert
	    val missingParams = table.columns.filter(_.isRequiredInInsert).filterNot(
	            c => atts(c.name).isDefined)
	    if (missingParams.isEmpty)
	    {
	        implicit val connection = context.connection
	        implicit val settings = context.settings
	        
	        // TODO: Return result instead
	        val model = DBModel(table, atts)
	        Try(model.insert()) match 
	        {
	            case Success(index) => 
	            {
	                model.index = index
	                Result.Success(model.immutableCopy(), Created).toResponse.withModifiedHeaders(
	                        _.withLocation((path/(index.toString())).toServerUrl))
	            }
	            case Failure(e) => Result.Failure(BadRequest, Some(e.getMessage)).toResponse
	        }
	    }
	    else
	        Result.Failure(BadRequest, Some("Requires parameters: " + 
	                missingParams.map(_.name).reduce(_ + ", " + _ )), 
	                Model(Vector("required" -> missingParams.map(_.name)))).toResponse
	}
	
	@deprecated("Use Relation class postFrom() instead", "v1.0")
	def postRelationTarget(relation: Relation, target: Table, sourceIndex: Value)
	        (implicit context: DBContext) = 
	{
	    // TODO: If last reference is from the target table (not to), finds the correct value*, 
	    // then inserts. Otherwise just inserts and keeps the index
	    // * This may require creation of the bridging rows
	    // Afterwards, (if last reference to target table), create the bridges / references
	    // This may require use of recursion (each bridged table at a time), which may make this 
	    // easier
	    // -> This doesn't work when, for example a bridge requires both of the indices -> 
	    // bridges should be created last
	    // This also creates a limitation that bridges may not have more than the 2 required 
	    // columns
	    // Also, this means that there can only be a single bridge per relation max (because 
	    // multiples can't be created at once)
	    // -> Change relation structure (actually, don't. Some relations simply aren't pushable)
	    // -> Also, remove type parameters from tableResource while you're at it
	    // Also... there should be a way to determine whether a bridge references a table or is 
	    // being referenced from one
	}
	
	
	
	/*
	def relatedResource2(relationName: String): Option[Tuple2[Relation, TableResource[_]]] = 
	{
	    val relation = relations.get(relationName)
	    val res = relation.flatMap(rel => TableResources.resourceForTable(rel.reference.to.table))
	    
	    if (res.isDefined)
	        relation.get -> res.get
	    else
	        None
	}*/
	
	/*
	private def findRelatedResource(relationName: String, myCondition: Condition) = 
	{
	    relations.get(relationName).flatMap
	    {
	        relation => 
	            // TODO: In order to continue, I need
	            // a) context (db connection, possibly list of available resources) to requests -- DONE
	            // b) follow(storable) -method to relation -- Let's implement it here instead
	            // c) which requires simple init to storableFactory trait -- DONE
	            // d) which requires storable model class (mutable?) -- DONE
	            
	            // Finds the targeted resource first
	            TableResources.resourceForTable(relation.to).flatMap
	            {
	                target => 
	                    
	                    // One to many references ...
	                    // TODO: How do you redirect for models, what about multiple models?
	                    // -> Make a StorableResource and storableResourceSet classes the request 
	                    //     can then be redirected to?
	                    
	                    // TODO: Also, here you should probably just return the target, 
	                    // then handle according to remaining path
	                    ???
	            }
	    }
	}
	*/
}