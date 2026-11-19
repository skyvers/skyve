package org.skyve.metadata.view.model.list;

import java.util.Date;

import org.locationtech.jts.geom.Geometry;
import org.skyve.CORE;
import org.skyve.domain.types.Decimal;
import org.skyve.metadata.user.User;
import org.skyve.persistence.SQL;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

public class SQLFilter implements Filter {
	@SuppressWarnings("unused")
	private SQL detailSQL;
	@SuppressWarnings("unused")
	private SQL summarySQL;
	@SuppressWarnings("unused")
	private User user;
	private boolean empty = true;
	
	SQLFilter(@Nullable SQL detailSQL, @Nullable SQL summarySQL) {
		this.detailSQL = detailSQL;
		this.summarySQL = summarySQL;
		user = CORE.getUser();
	}
	
	@Override
	public void addAnd(@Nonnull Filter filter) {
		empty = false;
/*
		detailFilter.addAnd(((SQLFilter) filter).detailFilter);
		if (summaryFilter != null) {
			summaryFilter.addAnd(((SQLFilter) filter).summaryFilter);
		}
*/
	}

	@Override
	public void addOr(@Nonnull Filter filter) {
		empty = false;
/*
		detailFilter.addOr(((SQLFilter) filter).detailFilter);
		if (summaryFilter != null) {
			summaryFilter.addOr(((SQLFilter) filter).summaryFilter);
		}
*/
	}

	@Override
	public void addTagged(@Nonnull String tagId, boolean tagged) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("add tagged {} with tagId {}", Boolean.valueOf(tagged), tagId);
		StringBuilder sb = new StringBuilder(64);
		sb.append("exists (select 1 from adminTagged as tagged where tagged.tag.bizId = '");
		sb.append(tagId);
		sb.append("' and tagged.bizUserId = '");
		sb.append(user.getId());
		sb.append("' and tagged.taggedBizId = bean.bizId)");

		if (tagged) {
			detailFilter.addExpression(sb.toString());
			if (summaryFilter != null) {
				summaryFilter.addExpression(sb.toString());
			}
		}
		else {
			sb.insert(0, "not ");
			detailFilter.addExpression(sb.toString());
			if (summaryFilter != null) {
				summaryFilter.addExpression(sb.toString());
			}
		}
*/
	}

	@Override
	public void addNull(@Nonnull String binding) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} is null", binding);
		detailFilter.addNull(binding);
		if (summaryFilter != null) {
			summaryFilter.addNull(binding);
		}
*/
	}

	@Override
	public void addNotNull(@Nonnull String binding) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} is not null", binding);
		detailFilter.addNotNull(binding);
		if (summaryFilter != null) {
			summaryFilter.addNotNull(binding);
		}
*/
	}

	@Override
	public void addEquals(@Nonnull String binding, @Nonnull String value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} equals {}", binding, value);
		detailFilter.addEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addEquals(binding, value);
		}
*/
	}

	@Override
	public void addEquals(@Nonnull String binding, @Nonnull Date value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} equals {}", binding, (value == null) ? "null" : value.toString());
		detailFilter.addEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addEquals(binding, value);
		}
*/
	}

	@Override
	public void addEquals(@Nonnull String binding, @Nonnull Integer value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} equals {}", binding, (value == null) ? "null" : value.toString());
		detailFilter.addEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addEquals(binding, value);
		}
*/
	}

	@Override
	public void addEquals(@Nonnull String binding, @Nonnull Long value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} equals {}", binding, (value == null) ? "null" : value.toString());
		detailFilter.addEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addEquals(binding, value);
		}
*/
	}

	@Override
	public void addEquals(@Nonnull String binding, @Nonnull Decimal value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} equals {}", binding, (value == null) ? "null" : value.toString());
		detailFilter.addEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addEquals(binding, value);
		}
*/
	}

	@Override
	public void addEquals(@Nonnull String binding, @Nonnull Boolean value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} equals {}", binding, (value == null) ? "null" : value.toString());
		detailFilter.addEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addEquals(binding, value);
		}
*/
	}

	@Override
	public void addEquals(@Nonnull String binding, @Nonnull Enum<?> value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} equals {}", binding, (value == null) ? "null" : value.toString());
		detailFilter.addEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addEquals(binding, value);
		}	
*/
	}

	@Override
	public void addEquals(@Nonnull String binding, @Nonnull Geometry value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} equals {}", binding, (value == null) ? "null" : value.toString());
		detailFilter.addEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addEquals(binding, value);
		}
*/
	}

	@Override
	public void addNotEquals(@Nonnull String binding, @Nonnull String value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} notEquals {}", binding, (value == null) ? "null" : value);
		detailFilter.addNotEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addNotEquals(binding, value);
		}
*/
	}

	@Override
	public void addNotEquals(@Nonnull String binding, @Nonnull Date value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} notEquals {}", binding, (value == null) ? "null" : value.toString());
		detailFilter.addNotEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addNotEquals(binding, value);
		}
*/
	}

	@Override
	public void addNotEquals(@Nonnull String binding, @Nonnull Integer value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} notEquals {}", binding, (value == null) ? "null" : value.toString());
		detailFilter.addNotEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addNotEquals(binding, value);
		}
*/
	}

	@Override
	public void addNotEquals(@Nonnull String binding, @Nonnull Long value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} notEquals {}", binding, (value == null) ? "null" : value.toString());
		detailFilter.addNotEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addNotEquals(binding, value);
		}
*/
	}

	@Override
	public void addNotEquals(@Nonnull String binding, @Nonnull Decimal value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} notEquals {}", binding, (value == null) ? "null" : value.toString());
		detailFilter.addNotEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addNotEquals(binding, value);
		}
*/
	}

	@Override
	public void addNotEquals(@Nonnull String binding, @Nonnull Boolean value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} notEquals {}", binding, (value == null) ? "null" : value.toString());
		detailFilter.addNotEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addNotEquals(binding, value);
		}
*/
	}

	@Override
	public void addNotEquals(@Nonnull String binding, @Nonnull Enum<?> value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} notEquals {}", binding, (value == null) ? "null" : value.toString());
		detailFilter.addNotEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addNotEquals(binding, value);
		}
*/
	}

	@Override
	public void addNotEquals(@Nonnull String binding, @Nonnull Geometry value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} notEquals {}", binding, (value == null) ? "null" : value.toString());
		detailFilter.addNotEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addNotEquals(binding, value);
		}
*/
	}

	@Override
	public void addEqualsIgnoreCase(@Nonnull String binding, @Nonnull String value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} equalsIgnoreCase {}", binding, (value == null) ? "null" : value);
		detailFilter.addLike(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLike(binding, value);
		}
*/
	}

	@Override
	public void addNotEqualsIgnoreCase(@Nonnull String binding, @Nonnull String value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} notEqualsIgnoreCase {}", binding, (value == null) ? "null" : value);
		detailFilter.addNotLike(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addNotLike(binding, value);
		}
*/
	}

	@Override
	public void addContains(@Nonnull String binding, @Nonnull String value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} contains {}", binding, (value == null) ? "null" : value);
		String operand = new StringBuilder(32).append('%').append(value).append('%').toString();
		detailFilter.addLike(binding, operand);
		if (summaryFilter != null) {
			summaryFilter.addLike(binding, operand);
		}
*/
	}

	@Override
	public void addNotContains(@Nonnull String binding, @Nonnull String value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} notContains {}", binding, (value == null) ? "null" : value);
		String operand = new StringBuilder(32).append('%').append(value).append('%').toString();
		detailFilter.addNullOrNotLike(binding, operand);
		if (summaryFilter != null) {
			summaryFilter.addNullOrNotLike(binding, operand);
		}
*/
	}

	@Override
	public void addStartsWith(@Nonnull String binding, @Nonnull String value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} startsWith {}", binding, (value == null) ? "null" : value);
		String operand = new StringBuilder(32).append(value).append('%').toString();
		detailFilter.addLike(binding, operand);
		if (summaryFilter != null) {
			summaryFilter.addLike(binding, operand);
		}
*/
	}

	@Override
	public void addNotStartsWith(@Nonnull String binding, @Nonnull String value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} notStartsWith {}", binding, (value == null) ? "null" : value);
		String operand = new StringBuilder(32).append(value).append('%').toString();
		detailFilter.addNullOrNotLike(binding, operand);
		if (summaryFilter != null) {
			summaryFilter.addNullOrNotLike(binding, operand);
		}
*/
	}

	@Override
	public void addEndsWith(@Nonnull String binding, @Nonnull String value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} endsWith {}", binding, (value == null) ? "null" : value);
		String operand = new StringBuilder(32).append('%').append(value).toString();
		detailFilter.addLike(binding, operand);
		if (summaryFilter != null) {
			summaryFilter.addLike(binding, operand);
		}
*/
	}

	@Override
	public void addNotEndsWith(@Nonnull String binding, @Nonnull String value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} notEndsWith {}", binding, (value == null) ? "null" : value);
		String operand = new StringBuilder(32).append('%').append(value).toString();
		detailFilter.addNullOrNotLike(binding, operand);
		if (summaryFilter != null) {
			summaryFilter.addNullOrNotLike(binding, operand);
		}
*/
	}

	@Override
	public void addGreaterThan(@Nonnull String binding, @Nonnull String value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} greaterThan {}", binding, (value == null) ? "null" : value);
		detailFilter.addGreaterThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThan(binding, value);
		}
*/
	}

	@Override
	public void addGreaterThan(@Nonnull String binding, @Nonnull Date value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} greaterThan {}", binding, (value == null) ? "null" : value.toString());
		detailFilter.addGreaterThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThan(binding, value);
		}
*/
	}

	@Override
	public void addGreaterThan(@Nonnull String binding, @Nonnull Integer value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} greaterThan {}", binding, (value == null) ? "null" : value.toString());
		detailFilter.addGreaterThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThan(binding, value);
		}
*/
	}

	@Override
	public void addGreaterThan(@Nonnull String binding, @Nonnull Long value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} greaterThan {}", binding, (value == null) ? "null" : value.toString());
		detailFilter.addGreaterThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThan(binding, value);
		}
*/
	}

	@Override
	public void addGreaterThan(@Nonnull String binding, @Nonnull Decimal value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} greaterThan {}", binding, (value == null) ? "null" : value.toString());
		detailFilter.addGreaterThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThan(binding, value);
		}
*/
	}

	@Override
	public void addGreaterThanOrEqualTo(@Nonnull String binding, @Nonnull String value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} greaterThanOrEqualTo {}", binding, (value == null) ? "null" : value);
		detailFilter.addGreaterThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThanOrEqualTo(binding, value);
		}
*/
	}

	@Override
	public void addGreaterThanOrEqualTo(@Nonnull String binding, @Nonnull Date value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} greaterThanOrEqualTo {}", binding, (value == null) ? "null" : value.toString());
		detailFilter.addGreaterThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThanOrEqualTo(binding, value);
		}
*/
	}

	@Override
	public void addGreaterThanOrEqualTo(@Nonnull String binding, @Nonnull Integer value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} greaterThanOrEqualTo {}", binding, (value == null) ? "null" : value.toString());
		detailFilter.addGreaterThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThanOrEqualTo(binding, value);
		}
*/
	}

	@Override
	public void addGreaterThanOrEqualTo(@Nonnull String binding, @Nonnull Long value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} greaterThanOrEqualTo {}", binding, (value == null) ? "null" : value.toString());
		detailFilter.addGreaterThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThanOrEqualTo(binding, value);
		}
*/
	}

	@Override
	public void addGreaterThanOrEqualTo(@Nonnull String binding, @Nonnull Decimal value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} greaterThanOrEqualTo {}", binding, (value == null) ? "null" : value.toString());
		detailFilter.addGreaterThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThanOrEqualTo(binding, value);
		}
*/
	}

	@Override
	public void addLessThan(@Nonnull String binding, @Nonnull String value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} lessThan {}", binding, (value == null) ? "null" : value);
		detailFilter.addLessThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThan(binding, value);
		}
*/
	}

	@Override
	public void addLessThan(@Nonnull String binding, @Nonnull Date value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} lessThan {}", binding, (value == null) ? "null" : value.toString());
		detailFilter.addLessThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThan(binding, value);
		}
*/
	}

	@Override
	public void addLessThan(@Nonnull String binding, @Nonnull Integer value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} lessThan {}", binding, (value == null) ? "null" : value.toString());
		detailFilter.addLessThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThan(binding, value);
		}
*/
	}

	@Override
	public void addLessThan(@Nonnull String binding, @Nonnull Long value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} lessThan {}", binding, (value == null) ? "null" : value.toString());
		detailFilter.addLessThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThan(binding, value);
		}
*/
	}

	@Override
	public void addLessThan(@Nonnull String binding, @Nonnull Decimal value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} lessThan {}", binding, (value == null) ? "null" : value.toString());
		detailFilter.addLessThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThan(binding, value);
		}
*/
	}

	@Override
	public void addLessThanOrEqualTo(@Nonnull String binding, @Nonnull String value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} lessThanOrEqualTo {}", binding, (value == null) ? "null" : value);
		detailFilter.addLessThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThanOrEqualTo(binding, value);
		}
*/
	}

	@Override
	public void addLessThanOrEqualTo(@Nonnull String binding, @Nonnull Date value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} lessThanOrEqualTo {}", binding, (value == null) ? "null" : value.toString());
		detailFilter.addLessThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThanOrEqualTo(binding, value);
		}
*/
	}

	@Override
	public void addLessThanOrEqualTo(@Nonnull String binding, @Nonnull Integer value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} lessThanOrEqualTo {}", binding, (value == null) ? "null" : value.toString());
		detailFilter.addLessThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThanOrEqualTo(binding, value);
		}
*/
	}

	@Override
	public void addLessThanOrEqualTo(@Nonnull String binding, @Nonnull Long value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} lessThanOrEqualTo {}", binding, (value == null) ? "null" : value.toString());
		detailFilter.addLessThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThanOrEqualTo(binding, value);
		}
*/
	}

	@Override
	public void addLessThanOrEqualTo(@Nonnull String binding, @Nonnull Decimal value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} lessThanOrEqualTo {}", binding, (value == null) ? "null" : value.toString());
		detailFilter.addLessThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThanOrEqualTo(binding, value);
		}
*/
	}

	@Override
	public void addBetween(@Nonnull String binding, @Nonnull String start, @Nonnull String end) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s between %s and %s", 
																		binding, 
																		(start == null) ? "null" : start,
																		(end == null) ? "null" : end));
		detailFilter.addBetween(binding, start, end);
		if (summaryFilter != null) {
			summaryFilter.addBetween(binding, start, end);
		}
*/
	}

	@Override
	public void addBetween(@Nonnull String binding, @Nonnull Date start, @Nonnull Date end) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s between %s and %s", 
																		binding, 
																		(start == null) ? "null" : start.toString(),
																		(end == null) ? "null" : end.toString()));
		detailFilter.addBetween(binding, start, end);
		if (summaryFilter != null) {
			summaryFilter.addBetween(binding, start, end);
		}
*/
	}

	@Override
	public void addBetween(@Nonnull String binding, @Nonnull Integer start, @Nonnull Integer end) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s between %s and %s", 
																		binding, 
																		(start == null) ? "null" : start.toString(),
																		(end == null) ? "null" : end.toString()));
		detailFilter.addBetween(binding, start, end);
		if (summaryFilter != null) {
			summaryFilter.addBetween(binding, start, end);
		}
*/
	}

	@Override
	public void addBetween(@Nonnull String binding, @Nonnull Long start, @Nonnull Long end) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s between %s and %s", 
																		binding, 
																		(start == null) ? "null" : start.toString(),
																		(end == null) ? "null" : end.toString()));
		detailFilter.addBetween(binding, start, end);
		if (summaryFilter != null) {
			summaryFilter.addBetween(binding, start, end);
		}
*/
	}

	@Override
	public void addBetween(@Nonnull String binding, @Nonnull Decimal start, @Nonnull Decimal end) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s between %s and %s", 
																		binding, 
																		(start == null) ? "null" : start.toString(),
																		(end == null) ? "null" : end.toString()));
		detailFilter.addBetween(binding, start, end);
		if (summaryFilter != null) {
			summaryFilter.addBetween(binding, start, end);
		}
*/
	}

	@Override
	public void addIn(@Nonnull String binding, @Nonnull Object... values) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) {
			StringBuilder sb = new StringBuilder(20 + (7 * values.length));
			sb.append(binding).append(" in  ");
			for (Object value : values) {
				sb.append(value).append(", ");
			}
			sb.setLength(sb.length() - 2);
			UtilImpl.LOGGER.info(sb.toString());
		}
		if (values.length > 0) {
			detailFilter.addIn(binding, values);
		}
		else {
			// select nothing
			detailFilter.addNull(binding);
			detailFilter.addNotNull(binding);
		}
		if (summaryFilter != null) {
			if (values.length > 0) {
				summaryFilter.addIn(binding, values);
			}
			else {
				// select nothing
				summaryFilter.addNull(binding);
				summaryFilter.addNotNull(binding);
			}
		}
 */
	}
	
	@Override
	public void addNotIn(@Nonnull String binding, @Nonnull Object... values) {
		// Nothing to see yet
	}
	
	@Override
	public void addWithin(@Nonnull String binding, @Nonnull Geometry value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} within {}", binding, (value == null) ? "null" : value.toString());
		detailFilter.addWithin(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addWithin(binding, value);
		}
*/
	}

	@Override
	public void addContains(@Nonnull String binding, @Nonnull Geometry value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} contains {}", binding, (value == null) ? "null" : value.toString());
		detailFilter.addContains(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addContains(binding, value);
		}
*/
	}

	@Override
	public void addCrosses(@Nonnull String binding, @Nonnull Geometry value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} crosses {}", binding, (value == null) ? "null" : value.toString());
		detailFilter.addCrosses(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addCrosses(binding, value);
		}
*/
	}

	@Override
	public void addDisjoint(@Nonnull String binding, @Nonnull Geometry value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} disjoint {}", binding, (value == null) ? "null" : value.toString());
		detailFilter.addDisjoint(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addDisjoint(binding, value);
		}
*/
	}

	@Override
	public void addIntersects(@Nonnull String binding, @Nonnull Geometry value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} intersects {}", binding, (value == null) ? "null" : value.toString());
		detailFilter.addIntersects(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addIntersects(binding, value);
		}
*/
	}

	@Override
	public void addOverlaps(@Nonnull String binding, @Nonnull Geometry value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} overlaps {}", binding, (value == null) ? "null" : value.toString());
		detailFilter.addOverlaps(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addOverlaps(binding, value);
		}
*/
	}

	@Override
	public void addTouches(@Nonnull String binding, @Nonnull Geometry value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("{} touches {}", binding, (value == null) ? "null" : value.toString());
		detailFilter.addTouches(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addTouches(binding, value);
		}
*/
	}

	@Override
	public boolean isEmpty() {
		return empty;
	}
}
