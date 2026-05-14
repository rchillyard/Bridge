package com.phasmidsoftware.bridge.cards

/**
  * The behavior of this trait is to (eagerly) quit a trick (holding, sequence),
  * which is to say take the (lazy) promotions of a sequence and to promote them
  * eagerly according to the quitting of the current trick.
  *
  * @tparam X the underlying type.
  */
trait Quittable[X]:
  def quit: X

/**
  * Trait to describe behavior of a type which can experience the play of a card.
  *
  * For example, Holding, Sequence, etc. can have cards played.
  *
  * NOTE: in practice, this trait is implemented via hierarchy, not type-class.
  *
  * @tparam X the underlying type.
  */
trait Playable[X]:
  def play(cardPlay: CardPlay): X

/**
  * The behavior of this trait is to adjust for virtual promotions by considering
  * the partner's holding as cooperative.
  *
  * @tparam X the underlying type.
  */
trait Cooperative[X]:
  def cooperate(x: X): X

/**
  * The behavior of this trait is to reprioritize an X.
  *
  * @tparam X the underlying type.
  */
trait Reprioritizable[X]:
  def reprioritize: X

/**
  * Trait to model the property of being (heuristically) evaluated.
  */
trait Evaluatable:
  def evaluate: Double

/**
  * Trait to model the removal of an element by priority.
  */
trait Removable:
  //noinspection ScalaStyle
  def -(priority: Int): Removable