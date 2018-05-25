package utopia.alliance.rest

import utopia.flow.generic.ValueConversions._

import utopia.access.http.Method._
import utopia.nexus.result.Result._

import utopia.nexus.rest.Resource
import utopia.access.http.Method
import utopia.vault.model.Storable
import utopia.nexus.http.Path
import utopia.nexus.http.Request
import utopia.nexus.rest.Ready
import utopia.flow.datastructure.immutable.Value
import utopia.nexus.result.Result
import utopia.nexus.result.Result.Success
import utopia.flow.datastructure.immutable.Model
import utopia.vault.sql.Update
import utopia.vault.sql.Where
import utopia.vault.sql.Condition

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
    def follow(path: Path)(implicit context: DBContext) = Ready(Some(path))
    
    def toResponse(remainingPath: Option[Path])(implicit context: DBContext): utopia.nexus.http.Response = ???
    
    
    // OTHER    -----------------------------
    
    private def get(remainingPath: Option[Path]) = 
    {
        if (remainingPath.isDefined)
        {
            val name = remainingPath.get.head
            val baseValue = data.toModel(name)
            
            val finalVar = remainingPath.get.tail.map(getFromValue(name, baseValue, _)) getOrElse 
                    (name -> baseValue);
            
            Success(Model(Vector(finalVar)))
        }
        else
        {
            Success(data.toModel)
        }
    }
    
    private def put(remainingPath: Option[Path])(implicit context: DBContext) = 
    {
        if (remainingPath.isEmpty)
        {
            val updatedValues = context.request.parameters.filter(c => data.table.find(c.name).isDefined)
            if (updatedValues.isEmpty)
                NoOperation
            else
                data.indexCondition.map
                {
                    c => 
                        context.connection(Update(data.table, updatedValues) + Where(c))
                        // TODO: Should return the new version of the model
                        // For this we need not only storable, but also updatable
                }
            // Update(data.table, updatedValues) + Where.apply(data.)
        }
    }
    
    // Finds values from a value recursively
    private def getFromValue(name: String, value: Value, path: Path): Tuple2[String, Value] = 
    {
        val next = path.head
        
        // Uses integer if possible / necessary
        val nextInt = next.int
        val nextIntResult = nextInt.map(value.apply)
        if (nextIntResult.isDefined)
        {
            val nextName = name + "/" + nextInt.get
            path.tail.map(getFromValue(nextName, nextIntResult.get, _)) getOrElse 
                    (nextName -> nextIntResult.get)
        }
        else
        {
            // If no integer result was found, tries with string
            val nextResult = value(next)
            path.tail.map(getFromValue(next, nextResult, _)) getOrElse (next -> nextResult)
        }
    }
}