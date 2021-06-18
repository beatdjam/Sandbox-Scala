var i = 0
new Thread(() => {
  (1 to 100).foreach(_ => i += 1)
}).start()

new Thread(() => {
  (1 to 100).foreach(_ => i += 1)
}).start()

println(i) // 複数スレッドでの操作なので、このタイミングで200になっていることが確定しない

// ロックによる排他制御
new Thread(() => {
  (1 to 100).foreach(_ => i.synchronized(i += 1))
})

println(i)