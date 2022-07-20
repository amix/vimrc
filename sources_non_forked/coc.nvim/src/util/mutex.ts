'use strict'
export class Mutex {
  private tasks: (() => void)[] = []
  private count = 1

  private sched(): void {
    if (this.count > 0 && this.tasks.length > 0) {
      this.count--
      let next = this.tasks.shift()
      next()
    }
  }

  public get busy(): boolean {
    return this.count == 0
  }

  public acquire(): Promise<() => void> {
    return new Promise<() => void>(res => {
      let task = () => {
        let released = false
        res(() => {
          if (!released) {
            released = true
            this.count++
            this.sched()
          }
        })
      }
      this.tasks.push(task)
      process.nextTick(this.sched.bind(this))
    })
  }

  public use<T>(f: () => Promise<T>): Promise<T> {
    return this.acquire()
      .then(release => f()
        .then(res => {
          release()
          return res
        })
        .catch(err => {
          release()
          throw err
        }))
  }
}
