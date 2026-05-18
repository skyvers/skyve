package util;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Arrays;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.report.jasperreports.SkyveDataSource;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;

import modules.test.domain.AllAttributesPersistent;
import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JRField;
import net.sf.jasperreports.engine.design.JRDesignField;

class SkyveDataSourceH2Test extends AbstractH2Test {

	private AllAttributesPersistent bean1;
	private AllAttributesPersistent bean2;
	private AllAttributesPersistent bean3;
	private User user;
	private Persistence p;

	@BeforeEach
	void setUpBeans() {
		p = CORE.getPersistence();
		user = p.getUser();

		bean1 = AllAttributesPersistent.newInstance();
		bean1.setText("first");
		bean1 = p.save(bean1);

		bean2 = AllAttributesPersistent.newInstance();
		bean2.setText("second");
		bean2 = p.save(bean2);

		bean3 = AllAttributesPersistent.newInstance();
		bean3.setText("third");
		bean3 = p.save(bean3);
	}

	@SuppressWarnings("static-method")
	private JRField fieldWithName(String name) {
		JRDesignField f = new JRDesignField();
		f.setName(name);
		return f;
	}

	@SuppressWarnings("static-method")
	private JRField fieldWithDescription(String description) {
		JRDesignField f = new JRDesignField();
		f.setName(description); // name required; description is the formatted-binding path
		f.setDescription(description);
		return f;
	}

	@Test
	void iteratesMultipleBeansInOrder() throws JRException {
		SkyveDataSource ds = new SkyveDataSource(user, Arrays.asList(bean1, bean2, bean3));
		int count = 0;
		while (ds.next()) {
			count++;
		}
		assertEquals(3, count);
	}

	@Test
	void getFieldValueResolvesRawBinding() throws JRException {
		// name only (no description) -> raw BindUtil.get path
		SkyveDataSource ds = new SkyveDataSource(user, bean1);
		ds.next();
		Object result = ds.getFieldValue(fieldWithName("text"));
		assertThat(result, notNullValue());
		assertThat(result.toString(), is("first"));
	}

	@Test
	void getFieldValueResolvesFormattedBinding() throws JRException {
		// description set -> BindUtil.getDisplay path (formatted string)
		SkyveDataSource ds = new SkyveDataSource(user, bean2);
		ds.next();
		Object result = ds.getFieldValue(fieldWithDescription("text"));
		assertThat(result, notNullValue());
		assertThat(result.toString(), is("second"));
	}

	@Test
	void getFieldValueForTHIS() throws JRException {
		SkyveDataSource ds = new SkyveDataSource(user, bean1);
		ds.next();
		Object result = ds.getFieldValue(fieldWithDescription("THIS"));
		assertThat(result, is((Bean) bean1));
	}

	@Test
	void getFieldValueForUSER() throws JRException {
		SkyveDataSource ds = new SkyveDataSource(user, bean1);
		ds.next();
		Object result = ds.getFieldValue(fieldWithDescription("USER"));
		assertThat(result, is(user));
	}
}
