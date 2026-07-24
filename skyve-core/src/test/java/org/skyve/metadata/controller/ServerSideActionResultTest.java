package org.skyve.metadata.controller;

import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.skyve.domain.Bean;

class ServerSideActionResultTest {

	@Test
	@SuppressWarnings("static-method")
	void constructorSetsBean() {
		Bean bean = Mockito.mock(Bean.class);
		ServerSideActionResult<Bean> result = new ServerSideActionResult<>(bean);
		assertSame(bean, result.getBean());
	}

	@Test
	@SuppressWarnings("static-method")
	void setBeanRoundtrip() {
		ServerSideActionResult<Bean> result = new ServerSideActionResult<>(Mockito.mock(Bean.class));
		Bean bean = Mockito.mock(Bean.class);
		result.setBean(bean);
		assertSame(bean, result.getBean());
	}
}
