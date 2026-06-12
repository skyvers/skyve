package sail.admin.pf;

import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;

import util.JUnitMultiThreadedRunner;

@Ignore
@RunWith(JUnitMultiThreadedRunner.class)
public class PrimeFacesAdminPerformanceSailIT extends PrimeFacesAdminSailIT {

	@Test
	public void test1() throws Exception {
		runRepeatedlyAfterDelay(1000);
	}
	
	@Test
	public void test2() throws Exception {
		runRepeatedlyAfterDelay(2000);
	}

	@Test
	public void test3() throws Exception {
		runRepeatedlyAfterDelay(3000);
	}

	@Test
	public void test4() throws Exception {
		runRepeatedlyAfterDelay(4000);
	}	
	@Test
	public void test5() throws Exception {
		runRepeatedlyAfterDelay(5000);
	}
	@Test
	public void test6() throws Exception {
		runRepeatedlyAfterDelay(6000);
	}

	@Test
	public void test7() throws Exception {
		runRepeatedlyAfterDelay(7000);
	}

	@Test
	public void test8() throws Exception {
		runRepeatedlyAfterDelay(8000);
	}

	@Test
	public void test9() throws Exception {
		runRepeatedlyAfterDelay(9000);
	}

	@Test
	public void test10() throws Exception {
		runRepeatedlyAfterDelay(10000);
	}

	@Test
	public void test11() throws Exception {
		runRepeatedlyAfterDelay(11000);
	}
	
	@Test
	public void test12() throws Exception {
		runRepeatedlyAfterDelay(12000);
	}

	private void runRepeatedlyAfterDelay(long delay) throws Exception {
		Thread.sleep(delay);
		while (true) {
			test();
		}
	}
}
