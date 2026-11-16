package org.hibernate.hql.spi.id.inline;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.Arrays;
import java.util.Collections;

import org.hibernate.dialect.H2Dialect;
import org.hibernate.type.IntegerType;
import org.hibernate.type.StringType;
import org.hibernate.type.TypeFactory;
import org.hibernate.type.TypeResolver;
import org.hibernate.type.spi.TypeConfiguration;
import org.junit.jupiter.api.Test;

@SuppressWarnings({"static-method", "boxing"})
class InlineIdsInClauseBuilderTest {
	private static class LimitedInExpressionCountDialect extends H2Dialect {
		private final int inExpressionCountLimit;

		private LimitedInExpressionCountDialect(int inExpressionCountLimit) {
			this.inExpressionCountLimit = inExpressionCountLimit;
		}

		@Override
		public int getInExpressionCountLimit() {
			return inExpressionCountLimit;
		}
	}

	private enum StringLikeEnum {
		VALUE;

		@Override
		public String toString() {
			return "enum'o\\k";
		}
	}

	@Test
	void testEscapesSingleQuotesForStringIdentifierValues() {
		InlineIdsInClauseBuilder builder = new InlineIdsInClauseBuilder(
				new H2Dialect(),
				StringType.INSTANCE,
				typeResolver(),
				new String[] {"id"},
				Arrays.asList(new Object[] {"simple"}, new Object[] {"te'st"}));

		assertThat(builder.toStatement(), is("('simple'),('te''st')"));
	}

	@Test
	void testKeepsNumericIdentifierValuesUnquoted() {
		InlineIdsInClauseBuilder builder = new InlineIdsInClauseBuilder(
				new H2Dialect(),
				IntegerType.INSTANCE,
				typeResolver(),
				new String[] {"id"},
				Arrays.asList(new Object[] {1}, new Object[] {2}));

		assertThat(builder.toStatement(), is("(1),(2)"));
	}

	@Test
	void testRendersNullIdentifierValueAsSqlNull() {
		InlineIdsInClauseBuilder builder = new InlineIdsInClauseBuilder(
				new H2Dialect(),
				IntegerType.INSTANCE,
				typeResolver(),
				new String[] {"id"},
				Collections.singletonList(new Object[] {null}));

		assertThat(builder.toStatement(), is("(null)"));
	}

	@Test
	void testQuotesEnumIdentifierValues() {
		InlineIdsInClauseBuilder builder = new InlineIdsInClauseBuilder(
				new H2Dialect(),
				IntegerType.INSTANCE,
				typeResolver(),
				new String[] {"id"},
				Collections.singletonList(new Object[] {StringLikeEnum.VALUE}));

		assertThat(builder.toStatement(), is("('enum''o\\k')"));
	}

	@Test
	void testQuotesEmptyStringIdentifierValues() {
		InlineIdsInClauseBuilder builder = new InlineIdsInClauseBuilder(
				new H2Dialect(),
				StringType.INSTANCE,
				typeResolver(),
				new String[] {"id"},
				Collections.singletonList(new Object[] {""}));

		assertThat(builder.toStatement(), is("('')"));
	}

	@Test
	void testEscapesMultipleConsecutiveSingleQuotes() {
		InlineIdsInClauseBuilder builder = new InlineIdsInClauseBuilder(
				new H2Dialect(),
				StringType.INSTANCE,
				typeResolver(),
				new String[] {"id"},
				Collections.singletonList(new Object[] {"a''b"}));

		assertThat(builder.toStatement(), is("('a''''b')"));
	}

	@Test
	void testPreservesBackslashesInStringValues() {
		InlineIdsInClauseBuilder builder = new InlineIdsInClauseBuilder(
				new H2Dialect(),
				StringType.INSTANCE,
				typeResolver(),
				new String[] {"id"},
				Collections.singletonList(new Object[] {"a\\b"}));

		assertThat(builder.toStatement(), is("('a\\b')"));
	}

	@Test
	void testHandlesCompositeIdentifierValues() {
		InlineIdsInClauseBuilder builder = new InlineIdsInClauseBuilder(
				new H2Dialect(),
				IntegerType.INSTANCE,
				typeResolver(),
				new String[] {"id1", "id2"},
				Arrays.asList(new Object[] {"a'b", 1}, new Object[] {"c", 2}));

		assertThat(builder.toStatement(), is("('a''b',1),('c',2)"));
	}

	@Test
	void testHandlesVeryLongIdentifierValues() {
		String veryLongValue = "a".repeat(4096) + "'" + "b".repeat(4096);
		InlineIdsInClauseBuilder builder = new InlineIdsInClauseBuilder(
				new H2Dialect(),
				StringType.INSTANCE,
				typeResolver(),
				new String[] {"id"},
				Collections.singletonList(new Object[] {veryLongValue}));

		assertThat(builder.toStatement(), is("('" + veryLongValue.replace("'", "''") + "')"));
	}

	@Test
	void testSplitsInlineIdsAcrossChunksWhenDialectDefinesInClauseLimit() {
		InlineIdsInClauseBuilder builder = new InlineIdsInClauseBuilder(
				new LimitedInExpressionCountDialect(2),
				IntegerType.INSTANCE,
				typeResolver(),
				new String[] {"id"},
				Arrays.asList(
						new Object[] {1},
						new Object[] {2},
						new Object[] {3},
						new Object[] {4},
						new Object[] {5}));

		String idFragment = builder.toStatement();
		assertThat(idFragment, is("(1),(2) ) or ( id ) in ((3),(4) ) or ( id ) in ((5)"));
		assertThat("(id) in (" + idFragment + ")", is("(id) in ((1),(2) ) or ( id ) in ((3),(4) ) or ( id ) in ((5))"));
	}

	// InlineIdsInClauseBuilder's constructor takes Hibernate 5's deprecated TypeResolver.
	@SuppressWarnings("deprecation")
	private static TypeResolver typeResolver() {
		TypeConfiguration typeConfiguration = new TypeConfiguration();
		return new TypeResolver(typeConfiguration, new TypeFactory(typeConfiguration));
	}
}
