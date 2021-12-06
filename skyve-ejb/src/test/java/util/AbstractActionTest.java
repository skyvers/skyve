package util;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ServerSideAction;

public abstract class AbstractActionTest<T extends Bean, A extends ServerSideAction<T>> extends AbstractH2TestForJUnit5 {

	protected abstract A getAction();

	protected abstract T getBean() throws Exception;

	@Test
	public void testExecute() throws Exception {
		try {
			getAction().execute(getBean(), null);
		} catch (ValidationException e) {
			// test pass - action validated incorrect input
		}
	}
}
