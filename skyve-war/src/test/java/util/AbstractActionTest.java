package util;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.metadata.controller.ServerSideAction;

public abstract class AbstractActionTest<T extends Bean, A extends ServerSideAction<T>> extends AbstractH2Test {

	protected abstract A getAction();

	protected abstract T getBean() throws Exception;

	@Test
	void testExecute() throws Exception {
		A action = getAction();
		T bean = getBean();
		Assertions.assertNotNull(action);
		Assertions.assertNotNull(bean);
		try {
			action.execute(bean, new MockWebContext());
		} catch (@SuppressWarnings("unused") ValidationException e) {
			// test pass - action validated incorrect input
		}
	}
}
