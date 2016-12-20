node {
  stage('Prereqs') {
  	sh('npm install -g elm@16.1')
  	sh('make deps')
  }
  stage('Build') {
    sh('make')
  }
}
