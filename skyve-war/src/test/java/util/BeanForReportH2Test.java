package util;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.impl.generate.jasperreports.BeanForReport;

import modules.test.domain.AllAttributesPersistent;

class BeanForReportH2Test extends AbstractH2Test {

	private String savedBizId;
	private Persistence p;

	@BeforeEach
	void setUpBean() throws Exception {
		p = CORE.getPersistence();
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		bean.setText("hello from BeanForReport");
		bean = p.save(bean);
		savedBizId = bean.getBizId();
	}

	@Test
	void getBeanRetrievesPersistedBean() {
		Bean result = BeanForReport.getBean(
				AllAttributesPersistent.MODULE_NAME,
				AllAttributesPersistent.DOCUMENT_NAME,
				savedBizId);
		assertThat(result, notNullValue());
		assertThat(((AllAttributesPersistent) result).getText(), is("hello from BeanForReport"));
	}

	@SuppressWarnings("static-method")
	@Test
	void getUserReturnsCurrentUser() {
		User user = BeanForReport.getUser();
		assertThat(user, notNullValue());
	}

	@Test
	void getMessageWithLiteralStringReturnsUnchanged() {
		Bean bean = BeanForReport.getBean(
				AllAttributesPersistent.MODULE_NAME,
				AllAttributesPersistent.DOCUMENT_NAME,
				savedBizId);
		String result = BeanForReport.getMessage(bean, "Static message");
		assertThat(result, is("Static message"));
	}

	@Test
	void getMessageWithBindingExpandsToValue() {
		Bean bean = BeanForReport.getBean(
				AllAttributesPersistent.MODULE_NAME,
				AllAttributesPersistent.DOCUMENT_NAME,
				savedBizId);
		String result = BeanForReport.getMessage(bean, "Value is {text}");
		assertThat(result, notNullValue());
		assertFalse(result.contains("{text}"), "binding placeholder should be expanded");
		assertThat(result, is("Value is hello from BeanForReport"));
	}

	@Test
	void getMessageFourArgWithBindingExpandsToValue() {
		String result = BeanForReport.getMessage(
				AllAttributesPersistent.MODULE_NAME,
				AllAttributesPersistent.DOCUMENT_NAME,
				savedBizId,
				"Value is {text}");
		assertThat(result, notNullValue());
		assertFalse(result.contains("{text}"), "binding placeholder should be expanded");
		assertThat(result, is("Value is hello from BeanForReport"));
	}

	@Test
	void evaluateConditionReturnsTrueForTrueCondition() {
		// AllAttributesPersistent has a condition named "condition" with expression: true
		boolean result = BeanForReport.evaluateCondition(
				AllAttributesPersistent.MODULE_NAME,
				AllAttributesPersistent.DOCUMENT_NAME,
				savedBizId,
				"condition");
		assertTrue(result);
	}
}
