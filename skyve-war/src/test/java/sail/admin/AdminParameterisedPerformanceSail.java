package sail.admin;

import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;
import org.junit.runners.Parameterized.UseParametersRunnerFactory;

import util.JUnitMultiThreadedRunnerParameterizedRunnerFactory;

@Ignore
@RunWith(Parameterized.class)
@UseParametersRunnerFactory(JUnitMultiThreadedRunnerParameterizedRunnerFactory.class)
public class AdminParameterisedPerformanceSail extends AdminSailIT {
	@Parameters
	public static String[][] params() {
		return new String[][] {{"demo", "admin", "admin"}};
	}

	@Parameter(0)
	public String customer = "demo";

	@Parameter(1)
	public String user = "admin";
	
	@Parameter(2)
	public String password = "admin";
	
	@Test
	@Override
	public void test() {
		selenium.login(customer, user, password);
	}
}
