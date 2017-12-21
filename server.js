const express = require('express')
const cors = require('cors')
const bodyParser = require('body-parser')
const app = express()
app.use(cors())
app.use(bodyParser.json())

let todoObj = []

app.get('/', (req, res) => {
  res.send(todoObj)
})

app.post('/update', (req, res) => {
  let {id, value} = req.body
  let todoToUpdate = todoObj.filter(todo => todo.id === id)[0]
  todoToUpdate.value = value
  res.send(todoToUpdate)
})

app.post('/delete', (req, res) => {
  let id = req.body.id
  todoObj = todoObj.filter(todo => todo.id != id)
  res.send({})
})

app.post('/add', (req, res) => {
  let date = Date.now()
  let item = req.body.todoItem
  console.log('this is item: ', item, req.body)
  let todoValue = {id: date, value: item}
  todoObj.push(todoValue)
  res.send(todoValue)
})

app.get('/error', (req, res) => {
  res.sendStatus(500)
})

app.listen(3000, (err) => {
  if (err) {
    console.log('Error in starting server: ', err)
  }
  else {
    console.log('Listening on PORT 3000')
  }
})
