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

/**
* This resource handles data in a single table, referencing other resources when necessary
* @author Mikko Hilpinen
* @since 22.5.2018
**/
class TableResource[T <: Storable](val factory: StorableFactory[T], val path: Path, 
        val relations: Map[String, Relation] = HashMap(), 
        val allowedMethods: Traversable[Method] = Vector(Get)) extends Resource
{
    def name = path.lastElement
	def follow(path: Path, request: Request)(implicit settings: ServerSettings): ResourceSearchResult = 
	{
        // If the path references another resource, redirects the request to that resource
        
        
        ???
    }
	def toResponse(request: Request, remainingPath: Option[Path])(implicit settings: ServerSettings): Response = ???
	
	private def findRelatedResource(relationName: String) = 
	{
	    relations.get(relationName).flatMap
	    {
	        relation => 
	            // TODO: In order to continue, I need
	            // a) context (db connection, possibly list of available resources) to requests
	            // b) follow(storable) -method to relation
	            // c) which requires simple init to storableFactory trait
	            // d) which requires storable model class (mutable?)
	            
	            ???
	    }
	}
}