package utopia.alliance.model

import utopia.vault.sql.Extensions._

import utopia.flow.datastructure.immutable.Value
import utopia.vault.model.Table

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