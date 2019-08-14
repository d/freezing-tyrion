workflow "New workflow" {
  on = "push"
  resolves = ["actions/checkout@master"]
}

action "actions/checkout@master" {
  uses = "actions/checkout@master"
}
