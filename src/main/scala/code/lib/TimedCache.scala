package org.plummtw.astgrail.lib

import java.util.Date
import collection.mutable.{LinkedList, HashMap, SynchronizedMap}

trait TimedCacheType {
  def clean(call_interval : Int) : Unit
}

class TimedCache[U, V](val timeout : Int) extends TimedCacheType{
  class TimedCacheItem[V](var data : V, var access_time : Date) {
    val lock = new Object()
    var invalidate = false
  }
  
  private var timeout_index = 0
  val map : HashMap[U, TimedCacheItem[V]] = new HashMap[U, TimedCacheItem[V]] with SynchronizedMap[U, TimedCacheItem[V]]
  
  GlobalTimedCache += this

  def get(key : U) : Option[V] = {
    val item = map.get(key)
    item match {
      case Some(x) if (!x.invalidate) => x.lock.synchronized {
                                       x.access_time = new Date()
                                       Some(x.data) }
      case _       => None
    }
  }

  def put(key : U, data : V) : Unit = {
    val item = map.get(key)
    item match {
      case Some(x) => x.lock.synchronized {
                        x.access_time = new Date()
                        x.invalidate = false
                        x.data = data
                      }
      case _       => map += key -> new TimedCacheItem(data, new Date)
    }
  }
  
  def invalidate(key : U) : Unit = {
    val item = map.get(key)
    item match {
      case Some(x) => x.lock.synchronized {
                        x.invalidate = false
                      }
      case _       => None
    }
  }
  
  def invalidateAll : Unit = {
    map.keys.foreach { key => invalidate(key) }
  }

  var removeNotifiers : List[(U, V)=> Unit] = List()

  def getOr(key : U)(func : () => V) : V = {
    get(key) match {
      case Some(x) => x
      case _       => val data = func()
                      put(key, data)
                      data
    }
  }
  
  def clean(call_interval : Int) : Unit = {
    timeout_index += call_interval
    if (timeout_index < timeout)
      return
    else
      timeout_index = 0
    
    val time_now = (new Date()).getTime()
    map.foreach { t =>
      val key  = t._1
      val data = t._2
      if (time_now - data.access_time.getTime() > timeout) {
        removeNotifiers.foreach (_(key, data.data))
        map.remove(key)
      }
    }
  }
}

object GlobalTimedCache {
  val TIMEOUT_CALL_INTERVAL = 1000
  
  var timed_cache_list: List[TimedCacheType] = List()
  
  def +=(that: TimedCacheType) = {
    timed_cache_list = timed_cache_list :+ that
    this
  }
  
  def -=(that: TimedCacheType) = {
    timed_cache_list = timed_cache_list filterNot (_ == that)
    this
  }
  
  def stop {
    is_thread_stop = true
  }

  var is_thread_stop = false

  val thread = new Thread {
    override def run() {
      while (!is_thread_stop) {
        Thread.sleep(TIMEOUT_CALL_INTERVAL)
        timed_cache_list.foreach {cache =>
          cache.clean(TIMEOUT_CALL_INTERVAL)
        }
      }
    }
  }

  thread.start()
}
