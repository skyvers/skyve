package org.skyve.metadata.controller;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;

class ServerSideActionResultTest {

	@Test
	@SuppressWarnings("static-method")
	void constructorSetsBean() {
		Bean bean = mock(Bean.class);
		ServerSideActionResult<Bean> result = new ServerSideActionResult<>(bean);
		assertSame(bean, result.getBean());
	}

	@Test
	@SuppressWarnings("static-method")
	void setBeanRoundtrip() {
		ServerSideActionResult<Bean> result = new ServerSideActionResult<>(mock(Bean.class));
		Bean bean = mock(Bean.class);
		result.setBean(bean);
		assertSame(bean, result.getBean());
	}
}
