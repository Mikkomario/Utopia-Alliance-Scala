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
import utopia.vault.model.Readable
import utopia.access.http.Forbidden
import utopia.access.http.MethodNotAllowed

/**
* These resources are used for handling request that concern a single model / row
* @author Mikko Hilpinen
* @since 23.5.2018
**/
class RowResource(val data: Readable, methods: Traversable[Method]) extends Resource[DBContext]
{
    // ATTRIBUTES    -----------------------
    
    // Resources cannot post more resources (that is handled in upper resources)
    val allowedMethods = methods.filterNot(_ == Post)
    
    
    // IMPLEMENTED -------------------------
    
    def name: String = data.index.stringOr()
    
    // The resource cannot forward requests
    def follow(path: Path)(implicit context: DBContext) = Ready(Some(path))
    
    def toResponse(remainingPath: Option[Path])(implicit context: DBContext) = 
    {
        val result = context.request.method match 
        {
            case Get => get(remainingPath)
            case Put => put(remainingPath)
            case Delete => delete(remainingPath)
            case Post => Failure(MethodNotAllowed)
        }
        result.toResponse
    }
    
    
    // OTHER    -----------------------------
    
    // Either gets value of a certain attribute or the whole model
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
            Success(data.toModel)
    }
    
    // Alters some columns
    private def put(remainingPath: Option[Path])(implicit context: DBContext): Result = 
    {
        implicit val connection = context.connection
        
        if (remainingPath.isEmpty)
        {
            if (data.setAndUpdate(context.request.parameters))
                Success(data.toModel)
            else
                NoOperation
        }
        else
            Failure(Forbidden, Some("PUT cannot target an attribute"))
    }
    
    // Deletes the whole object
    private def delete(remainingPath: Option[Path])(implicit context: DBContext): Result = 
    {
        implicit val connection = context.connection
        
        if (remainingPath.isEmpty)
        {
            data.delete()
            Empty
        }
        else
            Failure(Forbidden, Some("DELETE cannot target an attribute"))
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