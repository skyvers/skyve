pipeline {
  agent any

  tools {
    maven 'M3'
  }

  stages {
    stage('Build') {
      steps {
        git branch: 'master',
                    url: 'https://github.com/skyvers/skyve'
        sh 'mvn clean package'
        sh 'mvn -f skyve-war/pom.xml clean package'
      }
    }
  }

  post {
    always {
      archiveArtifacts artifacts: 'skyve-core/target/skyve-core-*.jar', fingerprint: true
      archiveArtifacts artifacts: 'skyve-ext/target/skyve-ext-*.jar', fingerprint: true
      archiveArtifacts artifacts: 'skyve-tools/target/skyve-tools-*.jar', fingerprint: true
      archiveArtifacts artifacts: 'skyve-web/target/skyve-web-*.jar', fingerprint: true
    }
  }
}