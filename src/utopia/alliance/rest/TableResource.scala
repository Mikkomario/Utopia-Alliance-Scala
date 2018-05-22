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
	def follow(path: Path, request: Request)(implicit settings: ServerSettings): ResourceSearchResult = ???
	def toResponse(request: Request, remainingPath: Option[Path])(implicit settings: ServerSettings): Response = ???
}