package util;

import org.junit.runner.Runner;
import org.junit.runners.model.InitializationError;
import org.junit.runners.parameterized.ParametersRunnerFactory;
import org.junit.runners.parameterized.TestWithParameters;

public class JUnitMultiTheadedRunnerParameterizedRunnerFactory implements ParametersRunnerFactory {
	@Override
	public Runner createRunnerForTestWithParameters(TestWithParameters test) throws InitializationError {
		return new JUnitMultiThreadedRunnerWithParameters(test);
	}
}
