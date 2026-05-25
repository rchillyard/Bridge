/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge

import com.phasmidsoftware.misc.JPredicate

package object director {
  type Pos = (Int, Card) // XXX Pair number and their "card"

  type Checker = JPredicate[ScoreVul]

}
