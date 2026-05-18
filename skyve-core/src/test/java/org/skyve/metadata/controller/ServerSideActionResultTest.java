package org.skyve.metadata.controller;

import static org.junit.jupiter.api.Assertions.assertNull;
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
	void constructorWithNullBean() {
		ServerSideActionResult<Bean> result = new ServerSideActionResult<>(null);
		assertNull(result.getBean());
	}

	@Test
	@SuppressWarnings("static-method")
	void setBeanRoundtrip() {
		ServerSideActionResult<Bean> result = new ServerSideActionResult<>(null);
		Bean bean = Mockito.mock(Bean.class);
		result.setBean(bean);
		assertSame(bean, result.getBean());
	}
}
