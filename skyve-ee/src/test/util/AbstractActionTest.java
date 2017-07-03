package util;

import org.junit.Before;
import org.junit.Test;
import org.skyve.domain.Bean;
import org.skyve.metadata.controller.ServerSideAction;

public abstract class AbstractActionTest<T extends Bean, A extends ServerSideAction<T>> extends AbstractH2Test {

	@Before
	public abstract void setUp() throws Exception;

	protected abstract A getAction();

	protected abstract T getBean() throws Exception;

	@Test
	public void testExceute() throws Exception {
		getAction().execute(getBean(), null);
	}
}
