package edu.knoldus

import scala.annotation.tailrec

class TotalCommission {

  def getTotalCommission[T >: Commission](commissionList: List[T]): (Int, Int, Int) = {

    lazy val (totalClientCommission, totalStreetCommission) = totalCommission(commissionList, 0, 0)

    @tailrec
    def totalCommission(commissionList: List[T], clientSum: Int, streetSum: Int): (Int, Int) = {

      commissionList match {
        case ClientSideCommission(commission) :: tail => totalCommission(tail, clientSum + commission, streetSum)
        case StreetSideCommission(commission) :: tail => totalCommission(tail, clientSum, streetSum + commission)
        case _ => (clientSum, streetSum)
      }

    }

    (totalClientCommission, totalStreetCommission, totalClientCommission + totalStreetCommission)
  }
}
