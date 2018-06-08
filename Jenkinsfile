pipeline {
  agent any

  tools {
    maven 'M3'
  }

  stages {
    stage('Build') {
      steps {
        git 'https://github.com/skyvers/skyve'
        sh 'mvn clean package'
      }
    }
  }

  post {
    always {
      archiveArtifacts artifacts: 'target/skyve-core-*.jar', fingerprint: true
      archiveArtifacts artifacts: 'target/skyve-ee-*.jar', fingerprint: true
      archiveArtifacts artifacts: 'target/skyve-ext-*.jar', fingerprint: true
      archiveArtifacts artifacts: 'target/skyve-tools-*.jar', fingerprint: true
      archiveArtifacts artifacts: 'target/skyve-web-*.jar', fingerprint: true
    }
  }
}