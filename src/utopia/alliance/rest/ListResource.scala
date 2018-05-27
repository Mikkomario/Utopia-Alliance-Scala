package utopia.alliance.rest

import utopia.flow.generic.ValueConversions._
import utopia.vault.sql.Extensions._

import utopia.access.http.Method._
import utopia.nexus.result.Result._

import utopia.nexus.rest.Resource
import utopia.access.http.Method
import utopia.vault.model.Table
import utopia.vault.model.Storable
import utopia.flow.generic.StringType
import utopia.flow.generic.IntType
import utopia.flow.generic.LongType
import utopia.flow.generic.DoubleType
import utopia.nexus.http.Path
import utopia.nexus.result.Result.Success
import utopia.flow.datastructure.immutable.Model
import utopia.nexus.result.Result.Failure
import utopia.access.http.Forbidden
import utopia.nexus.rest.Follow
import utopia.vault.model.Readable
import utopia.nexus.rest.Error
import utopia.vault.sql.ConditionElement
import utopia.vault.sql.Update
import utopia.vault.sql.Where

/**
* This resource represents a list of objects / rows
* @author Mikko Hilpinen
* @since 25.5.2018
**/
class ListResource(val name: String, val table: Table, val data: Seq[Readable], 
        methods: Traversable[Method] = Vector(Get)) extends Resource[DBContext]
{
    // ATTRIBUTES    ---------------------
    
    val allowedMethods = methods.filterNot(_ == Post)
    
    // private val indexType = data.head.index.dataType
    
    
    // COMPUTED    -----------------------
    
    /*
    private def hasStringIndex = indexType.isOfType(StringType)
    
    private def hasNumberIndex = indexType.isOfType(IntType) || indexType.isOfType(LongType) || 
                        indexType.isOfType(DoubleType)
    */
    
    private def indexCondition = table.primaryColumn.map(_ in data.map(_.index: ConditionElement))
    
    
    // IMPLEMENTED    --------------------
    
    def follow(path: Path)(implicit context: DBContext) = 
    {
        val next = path.head
        
        // If a singular instance is targeted, the query is directed for the targeted row
        val singleResult = findWithPosition(next) orElse findWithIndex(next)
        if (singleResult.isDefined)
            Follow(new RowResource(singleResult.get, allowedMethods), path.tail)
        else
        {
            // Next tries to find a segment from this list and follow with that
            // Otherwise has to return "Not Found"
            val range = findWithRange(next).map(new ListResource(name + "/" + next, table, _, methods))
            range.map(Follow(_, path.tail)) getOrElse Error()
        }
    }
    
    def toResponse(remainingPath: Option[Path])(implicit context: DBContext) = 
    {
        if (remainingPath.isEmpty)
        {
            // TODO: Add support for different methods
            Success(Model(Vector(name -> data.map(_.toModel).toVector))).toResponse
        }
        else 
            Error().toResult.toResponse
    }
    
    
    // OTHER    --------------------------
    
    private def get() = Success(Model(Vector(name -> data.map(_.toModel).toVector)))
    
    private def put()(implicit context: DBContext) = 
    {
        if (data.isEmpty)
            NoOperation
        else
        {
            // TODO: Return something else when index condition fails?
            // Updates the target rows, doesn't return anything
            // indexCondition.map(Update(table, context.request.parameters) + Where(_)).foreach(
            //        context.connection.apply);
            // TODO: Implement
        }
    }
    
    // For non-text type 'first' and 'last' are supported
    private def findWithPosition(next: String) = name match 
    {
        case "first" => data.headOption
        case "last" => data.lastOption
        case _ => None
    }
    
    // May search based on index (position)
    private def findWithIndex(next: String) = next.int.filter(i => i >= 0 && i < data.size).map(data.apply);
    
    // Range is available for all types
    private def findWithRange(next: String) = 
    {
        val parts = name.split("-")
        val min = parts(0).int
        val max = if (parts.length > 1) parts(1).int else None
        
        if (min.isDefined && max.isDefined) Some(data.drop(min.get).take(max.get - min.get)) else None
    }
}