package utopia.alliance.rest

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

/**
* This resource handles data in a single table, referencing other resources when necessary
* @author Mikko Hilpinen
* @since 22.5.2018
**/
class TableResource[+T <: Readable](val factory: StorableFactory[T], val path: Path, 
        val relations: Map[String, Relation] = HashMap(), 
        val allowedMethods: Traversable[Method] = Vector(Get)) extends Resource[DBContext]
{
    // COMPUTED    --------------------
    
    def table = factory.table
    
    def name = path.lastElement
	def follow(path: Path)(implicit context: DBContext): ResourceSearchResult = 
	{
        // If the path references another resource, redirects the request to that resource
        /*
        val targetResource = relatedResource(path.head)
        if (targetResource.isDefined)
        {
            
        }*/
        
        ???
    }
	def toResponse(remainingPath: Option[Path])(implicit context: DBContext): Response = ???
	
	// TODO: On post handling, also create the appropriate bridges
	// TODO: Handle ordering (including default order)
	// TODO: Create read-only versions for these resources
	
	/**
	 * @return a resource for the specified relation name. Also includes the relation itself
	 */
	def relatedResource(relationName: String) = relations.get(relationName).flatMap(relation => 
	        TableResources.resourceForTable(relation.reference.to.table).map(relation -> _));
	
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