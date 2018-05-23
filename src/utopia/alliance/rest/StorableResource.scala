package utopia.alliance.rest

import utopia.flow.generic.ValueConversions._

import utopia.access.http.Method._

import utopia.nexus.rest.Resource
import utopia.access.http.Method
import utopia.vault.model.Storable
import utopia.nexus.http.Path
import utopia.nexus.http.Request
import utopia.nexus.rest.Ready
import utopia.flow.datastructure.immutable.Value

/**
* These resources are used for handling request that concern a single model / row
* @author Mikko Hilpinen
* @since 23.5.2018
**/
class StorableResource(val data: Storable, methods: Traversable[Method]) extends Resource[DBContext]
{
    // ATTRIBUTES    -----------------------
    
    // Resources cannot post more resources (that is handled in upper resources)
    val allowedMethods = methods.filterNot(_ == Post)
    
    
    // IMPLEMENTED -------------------------
    
    def name: String = data.index.stringOr()
    
    // The resource cannot forward requests
    def follow(path: Path, request: Request)(implicit context: DBContext) = Ready(Some(path))
    
    def toResponse(request: Request, remainingPath: Option[Path])(implicit context: DBContext): utopia.nexus.http.Response = ???
    
    
    // OTHER    -----------------------------
    
    private def get(remainingPath: Option[Path]) = 
    {
        if (remainingPath.isDefined)
        {
            val name = remainingPath.get.head
            val baseValue = data.toModel(name)
            
            val finalValue = remainingPath.get.tail.map(getFromValue(baseValue, _)) getOrElse baseValue
            // TODO: Handle values better and wrap as a model with a single property
        }
        
        ???
    }
    
    // Finds values from a value recursively
    private def getFromValue(value: Value, path: Path): Value = 
    {
        val next = path.head
        // Uses integer if possible / necessary
        val nextValue = next.int.map(value.apply) getOrElse value(next)
        
        path.tail.map(getFromValue(nextValue, _)) getOrElse nextValue
    }
}