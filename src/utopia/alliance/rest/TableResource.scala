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

/**
* This resource handles data in a single table, referencing other resources when necessary
* @author Mikko Hilpinen
* @since 22.5.2018
**/
class TableResource[T <: Storable](val factory: StorableFactory[T], val path: Path, 
        val relations: Map[String, Relation] = HashMap(), 
        val allowedMethods: Traversable[Method] = Vector(Get)) extends Resource[DBContext]
{
    def name = path.lastElement
	def follow(path: Path, request: Request)(implicit context: DBContext): ResourceSearchResult = 
	{
        // If the path references another resource, redirects the request to that resource
        /*
        val targetResource = relatedResource(path.head)
        if (targetResource.isDefined)
        {
            
        }*/
        
        ???
    }
	def toResponse(request: Request, remainingPath: Option[Path])(implicit context: DBContext): Response = ???
	
	
	
	// private def relatedResource(relationName: String) = relations.get(relationName).flatMap(
	//        relation => TableResources.resourceForTable(relation.reference.to.table))
	
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