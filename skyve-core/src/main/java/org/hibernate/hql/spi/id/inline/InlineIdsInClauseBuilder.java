/*
 * Hibernate, Relational Persistence for Idiomatic Java
 *
 * License: GNU Lesser General Public License (LGPL), version 2.1 or later.
 * See the LICENSE file in the root directory or <http://www.gnu.org/licenses/lgpl-2.1.html>.
 *
 * Back-port note: Skyve local mitigation for CVE-2026-0603.
 */
package org.hibernate.hql.spi.id.inline;

import java.util.List;

import org.hibernate.dialect.Dialect;
import org.hibernate.type.StringType;
import org.hibernate.type.Type;
import org.hibernate.type.TypeResolver;

/**
 * Builds the where IN clause that wraps the identifiers to be updated/deleted.
 *
 * @author Vlad Mihalcea
 */
public class InlineIdsInClauseBuilder extends IdsClauseBuilder {

	private final int chunkLimit;

	public InlineIdsInClauseBuilder(
			Dialect dialect, Type identifierType, TypeResolver typeResolver, String[] columns, List<Object[]> ids) {
		super(dialect, identifierType, typeResolver, columns, ids);
		this.chunkLimit = dialect.getInExpressionCountLimit();
	}

	@Override
	public String toStatement() {
		StringBuilder buffer = new StringBuilder();

		String columnNames = String.join(",", (CharSequence[]) getColumns());

		for (int i = 0; i < getIds().size(); i++) {
			Object[] idTokens = getIds().get(i);
			if (i > 0) {
				if (chunkLimit > 0 && i % chunkLimit == 0) {
					buffer.append(" ) or ( ");
					buffer.append(columnNames);
					buffer.append(" ) in (");
				}
				else {
					buffer.append(",");
				}
			}
			buffer.append("(");
			appendSqlValuesToIdInClause(idTokens, buffer);
			buffer.append(")");
		}

		return buffer.toString();
	}

	private StringBuilder appendSqlValuesToIdInClause(Object[] values, StringBuilder buffer) {
		for (int i = 0; i < values.length; i++) {
			if (i > 0) {
				buffer.append(",");
			}
			appendSqlValueToIdInClause(values[i], buffer);
		}
		return buffer;
	}

	private StringBuilder appendSqlValueToIdInClause(Object value, StringBuilder buffer) {
		if (value == null) {
			return buffer.append("null");
		}

		Type identifierType = getIdentifierType();
		if ((identifierType instanceof StringType) || (value instanceof String) || value.getClass().isEnum()) {
			String valueAsString = value.toString();
			// SKYVE SECURITY PATCH (CVE-2026-0603):
			// This is the mitigation change in this class. We must escape embedded
			// single quotes by doubling them before rendering inline SQL literals.
			StringBuilder escapedValue = new StringBuilder(valueAsString.length());
			for (int i = 0; i < valueAsString.length(); i++) {
				char c = valueAsString.charAt(i);
				if (c == '\'') {
					escapedValue.append(c);
				}
				escapedValue.append(c);
			}

			buffer.append("'").append(escapedValue).append("'");
		}
		else {
			buffer.append(value);
		}

		return buffer;
	}
}
