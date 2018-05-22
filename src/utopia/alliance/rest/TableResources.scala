package utopia.alliance.rest

import scala.collection.immutable.HashSet
import utopia.vault.model.Table

/**
* This class keeps track of all different table resource in order to provide shared data
* @author Mikko Hilpinen
* @since 22.5.2018
**/
object TableResources
{
	// ATTRIBUTES    ---------------------
    
    private var resources = HashSet[TableResource[_]]()
    
    
    // COMPUTED    ----------------------
    
    /**
     * All of the introduced resources
     */
    def all = resources
    
    
    // OTHER    -------------------------
    
    /**
     * A resource for the provided table, if there is one
     */
    def resourceForTable(table: Table) = resources.find(_.factory.table == table)
    
    /*
     * A resource for a table with the provided name, if there is one
     */
    // def resourcesForTable(tableName: String) = resources.find(_.factory.table.name == tableName)
    
    /**
     * Introduces a new resource to this set
     */
    def introduce(resource: TableResource[_]) = resources += resource
    
    /**
     * Introduces multiple resources to this set
     */
    def introduce(first: TableResource[_], second: TableResource[_], more: TableResource[_]*) = 
            resources ++= first +: second +: more
}