package org.skyve.impl.report.jasperreports;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.skyve.domain.Bean;
import org.skyve.metadata.user.User;

import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JRField;

@ExtendWith(MockitoExtension.class)
class SkyveDataSourceTest {

	@Mock
	private User mockUser;

	@Mock
	private Bean mockBean;

	@Mock
	private JRField mockField;

	private SkyveDataSource source;

	@BeforeEach
	void setUp() {
		source = new SkyveDataSource(mockUser, Collections.singletonList(mockBean));
	}

	@Test
	void nextReturnsTrueForSingleBean() throws JRException {
		assertTrue(source.next());
		assertFalse(source.next());
	}

	@Test
	void nextReturnsFalseForEmptyList() throws JRException {
		SkyveDataSource empty = new SkyveDataSource(mockUser, Collections.emptyList());
		assertFalse(empty.next());
	}

	@Test
	void nextWithIteratorConstructor() throws JRException {
		SkyveDataSource iterSource = new SkyveDataSource(mockUser, Arrays.asList(mockBean, mockBean).iterator());
		assertTrue(iterSource.next());
		assertTrue(iterSource.next());
		assertFalse(iterSource.next());
	}

	@Test
	void nextWithSingleBeanConstructor() throws JRException {
		SkyveDataSource singleSource = new SkyveDataSource(mockUser, mockBean);
		assertTrue(singleSource.next());
		assertFalse(singleSource.next());
	}

	@Test
	void getFieldValueForTHISBinding() throws JRException {
		source.next(); // advance to first bean
		when(mockField.getDescription()).thenReturn("THIS");

		Object result = source.getFieldValue(mockField);
		assertThat(result, is(mockBean));
	}

	@Test
	void getFieldValueForUSERBinding() throws JRException {
		source.next();
		when(mockField.getDescription()).thenReturn("USER");

		Object result = source.getFieldValue(mockField);
		assertThat(result, is(mockUser));
	}

	@Test
	void getFieldValueNullDescriptionAndNullNameThrowsJRException() throws JRException {
		// When description is null and name is null, the else branch attempts BindUtil.get(bean, null)
		// which NPEs inside StringTokenizer. SkyveDataSource wraps all exceptions as JRException.
		SkyveDataSource ds = new SkyveDataSource(mockUser, mockBean);
		ds.next();

		JRField nullField = mock(JRField.class);
		when(nullField.getDescription()).thenReturn(null);
		when(nullField.getName()).thenReturn(null);

		assertThrows(JRException.class, () -> ds.getFieldValue(nullField));
	}

	@Test
	void getFieldValueWrapsExceptionAsJRException() throws JRException {
		source.next();
		when(mockField.getDescription()).thenReturn("nonExistentBinding");
		when(mockField.getName()).thenReturn("nonExistentBinding");

		// BindUtil.unsanitiseBinding will pass through, BindUtil.getDisplay on a mock bean
		// will throw because mockUser.getCustomer() returns null.
		// Verify we get a JRException (not a raw RuntimeException).
		assertThrows(JRException.class, () -> source.getFieldValue(mockField));
	}
}
