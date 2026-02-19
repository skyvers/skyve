package org.hibernate.hql.spi.id.inline;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.Arrays;

import org.hibernate.dialect.H2Dialect;
import org.hibernate.type.IntegerType;
import org.hibernate.type.StringType;
import org.hibernate.type.TypeFactory;
import org.hibernate.type.TypeResolver;
import org.hibernate.type.spi.TypeConfiguration;
import org.junit.Test;

public class InlineIdsInClauseBuilderTest {
	@Test
	public void testEscapesSingleQuotesForStringIdentifierValues() {
		InlineIdsInClauseBuilder builder = new InlineIdsInClauseBuilder(
				new H2Dialect(),
				StringType.INSTANCE,
				typeResolver(),
				new String[] {"id"},
				Arrays.asList(new Object[] {"simple"}, new Object[] {"te'st"}));

		assertThat(builder.toStatement(), is("('simple'),('te''st')"));
	}

	@Test
	public void testKeepsNumericIdentifierValuesUnquoted() {
		InlineIdsInClauseBuilder builder = new InlineIdsInClauseBuilder(
				new H2Dialect(),
				IntegerType.INSTANCE,
				typeResolver(),
				new String[] {"id"},
				Arrays.asList(new Object[] {1}, new Object[] {2}));

		assertThat(builder.toStatement(), is("(1),(2)"));
	}

	private static TypeResolver typeResolver() {
		TypeConfiguration typeConfiguration = new TypeConfiguration();
		return new TypeResolver(typeConfiguration, new TypeFactory(typeConfiguration));
	}
}
