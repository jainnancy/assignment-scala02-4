package edu.knoldus

import org.apache.log4j.Logger

abstract class Commission

case class ClientSideCommission(value: Int) extends Commission

case class StreetSideCommission(value: Int) extends Commission

class TotalCommission {

  def getTotalCommission[T >: Commission](commissionList: List[T]): (Int, Int, Int) = {

    val result = totalCommission(commissionList, List(), List())

    def totalCommission(commissionList: List[T], clientList: List[Int], streetList: List[Int]): (Int, Int) = {

      commissionList match {
        case head :: tail => {
          case ClientSideCommission(commission) => {
            totalCommission(tail, commission :: clientList, streetList)
          }
          case StreetSideCommission(commission) => {
            totalCommission(tail, clientList, commission :: streetList)
          }
          case _ => {
            (clientList.sum, streetList.sum)
          }
        }
      }
    }
    (result._1,result._2,result._1 + result._2)
  }
}

object AllValues {
  val commissionC1 = 111
  val commissionC2 = 97
  val commissionC3 = 187
  val commissionS1 = 76
  val commissionS2 = 86
  val commissionS3 = 121
  val client1 = ClientSideCommission(commissionC1)
  val client2 = ClientSideCommission(commissionC2)
  val client3 = ClientSideCommission(commissionC3)
  val street1 = StreetSideCommission(commissionS1)
  val street2 = StreetSideCommission(commissionS2)
  val street3 = StreetSideCommission(commissionS3)
}

object CalculateCommission extends App {
  val log = Logger.getLogger(this.getClass)

  private sealed trait CommissionDisplay {
    def totalDisplayCommission: String
  }

  private implicit class CommissionDisplayImplemented[T](commissionList: List[T]) extends CommissionDisplay {

    override def totalDisplayCommission: String = {
      val summedCommission = new TotalCommission
      val calculatedCommission = summedCommission.getTotalCommission(commissionList)
      val clientSideTotal = calculatedCommission._1
      val streetSideTotal = calculatedCommission._2
      val mingledTotal = calculatedCommission._3

      s"The total street commission is $streetSideTotal \n" +
        s"The total client commission is $clientSideTotal\n" +
        s"The total mingled commission is $mingledTotal"
    }
  }

  val lisOfCommissions = List(AllValues.client1, AllValues.street1, AllValues.client2,
    AllValues.client3, AllValues.street3, AllValues.street2)
  log.info(lisOfCommissions.totalDisplayCommission)
}
