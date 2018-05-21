package utopia.alliance.model

/**
* Relation types describe the different styles of relations objects may have with each other
* @author Mikko Hilpinen
* @since 21.5.2018
**/
sealed trait RelationType

/**
 * A relation from one object to multiple (0-n) other objects
 */
case object ToMany extends RelationType
/**
 * A relation from one object to exactly one other object
 */
case object ToOne extends RelationType
/**
 * A relation from one object to zero or one other object(s)
 */
case object ToSome extends RelationType