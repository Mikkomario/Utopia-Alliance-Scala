package utopia.alliance.rest

import utopia.flow.generic.ValueConversions._
import utopia.vault.sql.Extensions._

import utopia.access.http.Method._
import utopia.nexus.result.Result._

import utopia.vault.sql

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
import utopia.nexus.result.Result
import utopia.access.http.MethodNotAllowed
import utopia.vault.sql.Select
import utopia.nexus.rest.Ready

/**
* This resource represents a list of objects / rows
* @author Mikko Hilpinen
* @since 25.5.2018
**/
class ListResource(val name: String, val tableResource: TableResource[Readable], 
        val data: Seq[Readable]) extends Resource[DBContext]
{
    // ATTRIBUTES    ---------------------
    
    val allowedMethods = tableResource.allowedMethods.filterNot(_ == Post)
    
    
    // COMPUTED    -----------------------
    
    /**
     * The table used by this resource
     */
    def table = tableResource.table
    
    private def indexCondition = tableResource.factory.table.primaryColumn.map(
            _ in data.map(_.index: ConditionElement))
    
    
    // IMPLEMENTED    --------------------
    
    def follow(path: Path)(implicit context: DBContext) = 
    {
        val next = path.head
        
        // If a reference is targeted, directs the query to another resource list
        val referenceResult = findWithReference(next)
        if (referenceResult.isDefined)
            Follow(referenceResult.get, path.tail)
        else
        {
            // If a singular instance is targeted, the query is directed for the targeted row
            val singleResult = findWithPosition(next) orElse findWithIndex(next)
            if (singleResult.isDefined)
                Follow(new RowResource(singleResult.get, tableResource), path.tail)
            else
            {
                // Next tries to find a segment from this list and follow with that
                // Otherwise has to return "Not Found" (unless targeting a specific attribute 
                // for the resources)
                val range = findWithRange(next).map(new ListResource(name + "/" + next, tableResource, _))
                range.map(Follow(_, path.tail)).getOrElse(
                        if (path.length < 2) Ready(Some(path)) else Error())
            }
        }
    }
    
    def toResponse(remainingPath: Option[Path])(implicit context: DBContext) = 
    {
        if (remainingPath.isEmpty)
        {
            val result = context.request.method match 
            {
                case Get => get()
                case Put => put()
                case Delete => delete()
                case _ => Failure(MethodNotAllowed)
            }
            
            result.toResponse
        }
        else if (remainingPath.get.length < 2)
        {
            val next = remainingPath.get.head
            if (table.contains(next))
            {
                if (context.request.method == Get)
                    Success(Model(Vector(name + "/" + next -> 
                            data.map(_.toModel(next)).filter(_.isDefined).toVector))).toResponse
                else
                    Error(MethodNotAllowed).toResult.toResponse
            }
            else
                Error().toResult.toResponse
        }
        else
            Error().toResult.toResponse
    }
    
    
    // OTHER    --------------------------
    
    private def get() = Success(Model(Vector(name -> data.map(_.toModel).toVector)))
    
    private def put()(implicit context: DBContext): Result = 
    {
        implicit val connection = context.connection
        
        if (data.isEmpty)
            NoOperation
        else
        {
            val condition = indexCondition
            
            if (condition.isDefined)
            {
                val update = Update(table, context.request.parameters).map(_ + Where(condition.get))
                update.foreach(_.execute())
                if (update.isDefined) Empty else NoOperation
            }
            else
                Failure(Forbidden, Some("Cannot update items from tables withoud index"))
        }
    }
    
    private def delete()(implicit context: DBContext): Result = 
    {
        implicit val connection = context.connection
        
        if (data.isEmpty)
            NoOperation
        else
        {
            val delete = indexCondition.map(sql.Delete(table) + Where(_))
            delete.foreach(_.execute())
            if (delete.isDefined) Empty else Failure(Forbidden, Some(
                    "Cannot delete items from tables without index"))
        }
    }
    
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
    
    private def findWithReference(refName: String)(implicit context: DBContext) = 
    {
        tableResource.relatedResource(refName).flatMap
        {
            case (reference, target) => 
                
                val select = indexCondition.map(
                        Select(reference.toSqlTarget, target.table.columns) + Where(_));
                val data = select.map(context.connection.apply).map(_.rows.map(_.toModel).flatMap(target.factory.apply))
                data.map(new ListResource(refName, target, _))    
        }
    }
}