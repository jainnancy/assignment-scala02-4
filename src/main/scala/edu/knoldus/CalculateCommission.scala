package edu.knoldus

import org.apache.log4j.Logger

abstract class Commission(value: Int)

case class ClientSideCommission(value: Int) extends Commission(value)

case class StreetSideCommission(value: Int) extends Commission(value)

object CalculateCommission extends App {

  val log = Logger.getLogger(this.getClass)
  val lisOfCommissions = List(AllValues.street1, AllValues.client1)

  log.info(s"Input: $lisOfCommissions\n\nOutput:\n")
  log.info(lisOfCommissions.totalDisplayCommission)

  private sealed trait CommissionDisplay {
    def totalDisplayCommission: String
  }

  private implicit class CommissionDisplayImplemented[T](commissionList: List[T]) extends CommissionDisplay {

    override def totalDisplayCommission: String = {
      val summedCommission = new TotalCommission
      val (clientSideTotal, streetSideTotal, mingledTotal) = summedCommission.getTotalCommission(commissionList)

      s"The total street commission is $streetSideTotal \n" +
        s"The total client commission is $clientSideTotal\n" +
        s"The total mingled commission is $mingledTotal"
    }

  }

}
