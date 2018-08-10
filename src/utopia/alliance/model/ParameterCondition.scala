package utopia.alliance.model

import utopia.vault.sql.Extensions._

import utopia.flow.datastructure.immutable.Value
import utopia.vault.model.Table
import utopia.flow.datastructure.template.Property

object ParameterCondition
{
    /**
     * Creates an equality condition based on a constant value
     */
    def equality(c: Property) = new ParameterCondition(c.name, Equals, c.value)
}

/**
* Parameter conditions are used for converting query parameters to sql level query conditions
* @author Mikko Hilpinen
* @since 8.8.2018
**/
class ParameterCondition(val varName: String, val comparison: Comparison, val comparedValue: Value)
{
	// COMPUTED    ---------------------
    
    def toSqlConditionFor(table: Table) = table.find(varName).map(
            c => comparison.toSqlCondition(c, comparedValue));
}