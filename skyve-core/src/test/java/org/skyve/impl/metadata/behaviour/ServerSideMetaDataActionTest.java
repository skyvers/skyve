package org.skyve.impl.metadata.behaviour;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.repository.behaviour.ActionMetaData;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

@ExtendWith(MockitoExtension.class)
class ServerSideMetaDataActionTest {

	@Mock
	private ActionMetaData actionMetaData;

	@Mock
	private Bean bean;

	@Mock
	private WebContext webContext;

	@Test
	void executeCallsMetaDataActionAndReturnsBean() throws Exception {
		ServerSideMetaDataAction action = new ServerSideMetaDataAction(actionMetaData);
		ServerSideActionResult<Bean> result = action.execute(bean, webContext);
		Mockito.verify(actionMetaData).execute(bean);
		assertNotNull(result);
		assertSame(bean, result.getBean());
	}
}
