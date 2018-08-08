package utopia.alliance.model

import utopia.vault.sql.Condition
import utopia.vault.sql.ConditionElement

/**
* Comparisons are used for specifying how parameter values are compared with matching data
* @author Mikko Hilpinen
* @since 8.8.2018
**/
sealed trait Comparison
{
    // ABSTRACT    ------------------
    
    /**
     * Creates an sql condition based on this single comparison
     */
    def toSqlCondition(first: ConditionElement, second: ConditionElement): Condition
}

/**
 * Checks if two values are considered equal
 */
case object Equals extends Comparison
{
    def toSqlCondition(first: ConditionElement, second: ConditionElement) = first <=> second
}

/**
 * Checks if the first value is smaller than the second value
 */
case object Smaller extends Comparison
{
    def toSqlCondition(first: ConditionElement, second: ConditionElement) = first < second
}

/**
 * Checks if the first value is larger than the second value
 */
case object Larger extends Comparison
{
    def toSqlCondition(first: ConditionElement, second: ConditionElement) = first > second
}

/**
 * Checks if the first value is smaller than or equal to the second value
 */
case object EqualsOrSmaller extends Comparison
{
    def toSqlCondition(first: ConditionElement, second: ConditionElement) = first <= second
}

/**
 * Checks if the first value is larger than or equal to the second value
 */
case object EqualsOrLarger extends Comparison
{
    def toSqlCondition(first: ConditionElement, second: ConditionElement) = first >= second
}