package org.skyve.impl.report.jasperreports;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
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
public class SkyveDataSourceTest {

	@Mock
	private User mockUser;

	@Mock
	private Bean mockBean;

	@Mock
	private JRField mockField;

	private SkyveDataSource source;

	@BeforeEach
	public void setUp() {
		source = new SkyveDataSource(mockUser, Collections.singletonList(mockBean));
	}

	@Test
	public void nextReturnsTrueForSingleBean() throws JRException {
		assertThat(source.next(), is(true));
		assertThat(source.next(), is(false));
	}

	@Test
	public void nextReturnsFalseForEmptyList() throws JRException {
		SkyveDataSource empty = new SkyveDataSource(mockUser, Collections.emptyList());
		assertThat(empty.next(), is(false));
	}

	@Test
	public void nextWithIteratorConstructor() throws JRException {
		SkyveDataSource iterSource = new SkyveDataSource(mockUser, Arrays.asList(mockBean, mockBean).iterator());
		assertThat(iterSource.next(), is(true));
		assertThat(iterSource.next(), is(true));
		assertThat(iterSource.next(), is(false));
	}

	@Test
	public void nextWithSingleBeanConstructor() throws JRException {
		SkyveDataSource singleSource = new SkyveDataSource(mockUser, mockBean);
		assertThat(singleSource.next(), is(true));
		assertThat(singleSource.next(), is(false));
	}

	@Test
	public void getFieldValueForTHISBinding() throws JRException {
		source.next(); // advance to first bean
		when(mockField.getDescription()).thenReturn("THIS");

		Object result = source.getFieldValue(mockField);
		assertThat(result, is(mockBean));
	}

	@Test
	public void getFieldValueForUSERBinding() throws JRException {
		source.next();
		when(mockField.getDescription()).thenReturn("USER");

		Object result = source.getFieldValue(mockField);
		assertThat(result, is(mockUser));
	}

	@Test
	public void getFieldValueNullDescriptionAndNullNameThrowsJRException() throws JRException {
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
	public void getFieldValueWrapsExceptionAsJRException() throws JRException {
		source.next();
		when(mockField.getDescription()).thenReturn("nonExistentBinding");
		when(mockField.getName()).thenReturn("nonExistentBinding");

		// BindUtil.unsanitiseBinding will pass through, BindUtil.getDisplay on a mock bean
		// will throw because mockUser.getCustomer() returns null.
		// Verify we get a JRException (not a raw RuntimeException).
		assertThrows(JRException.class, () -> source.getFieldValue(mockField));
	}
}
