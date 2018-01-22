package org.skyve.metadata.view.model.list;

import java.util.Date;

import org.skyve.CORE;
import org.skyve.domain.types.Decimal;
import org.skyve.metadata.user.User;
import org.skyve.persistence.SQL;

import com.vividsolutions.jts.geom.Geometry;

public class SQLFilter implements Filter {
	private SQL detailSQL;
	private SQL summarySQL;
	private User user;
	private boolean empty = true;
	
	SQLFilter(SQL detailSQL, SQL summarySQL) {
		this.detailSQL = detailSQL;
		this.summarySQL = summarySQL;
		user = CORE.getUser();
	}
	
	@Override
	public void addAnd(Filter filter) {
		empty = false;
/*
		detailFilter.addAnd(((SQLFilter) filter).detailFilter);
		if (summaryFilter != null) {
			summaryFilter.addAnd(((SQLFilter) filter).summaryFilter);
		}
*/
	}

	@Override
	public void addOr(Filter filter) {
		empty = false;
/*
		detailFilter.addOr(((SQLFilter) filter).detailFilter);
		if (summaryFilter != null) {
			summaryFilter.addOr(((SQLFilter) filter).summaryFilter);
		}
*/
	}

	@Override
	public void addTagged(String tagId, boolean tagged) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("add tagged %b with tagId %s", Boolean.valueOf(tagged), tagId));
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
	public void addNull(String binding) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s is null", binding));
		detailFilter.addNull(binding);
		if (summaryFilter != null) {
			summaryFilter.addNull(binding);
		}
*/
	}

	@Override
	public void addNotNull(String binding) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s is not null", binding));
		detailFilter.addNotNull(binding);
		if (summaryFilter != null) {
			summaryFilter.addNotNull(binding);
		}
*/
	}

	@Override
	public void addEquals(String binding, String value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s equals %s", binding, value));
		detailFilter.addEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addEquals(binding, value);
		}
*/
	}

	@Override
	public void addEquals(String binding, Date value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s equals %s", binding, (value == null) ? "null" : value.toString()));
		detailFilter.addEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addEquals(binding, value);
		}
*/
	}

	@Override
	public void addEquals(String binding, Integer value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s equals %s", binding, (value == null) ? "null" : value.toString()));
		detailFilter.addEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addEquals(binding, value);
		}
*/
	}

	@Override
	public void addEquals(String binding, Long value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s equals %s", binding, (value == null) ? "null" : value.toString()));
		detailFilter.addEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addEquals(binding, value);
		}
*/
	}

	@Override
	public void addEquals(String binding, Decimal value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s equals %s", binding, (value == null) ? "null" : value.toString()));
		detailFilter.addEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addEquals(binding, value);
		}
*/
	}

	@Override
	public void addEquals(String binding, Boolean value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s equals %s", binding, (value == null) ? "null" : value.toString()));
		detailFilter.addEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addEquals(binding, value);
		}
*/
	}

	@Override
	public void addEquals(String binding, Enum<?> value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s equals %s", binding, (value == null) ? "null" : value.toString()));
		detailFilter.addEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addEquals(binding, value);
		}	
*/
	}

	@Override
	public void addEquals(String binding, Geometry value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s equals %s", binding, (value == null) ? "null" : value.toString()));
		detailFilter.addEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addEquals(binding, value);
		}
*/
	}

	@Override
	public void addNotEquals(String binding, String value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s notEquals %s", binding, (value == null) ? "null" : value));
		detailFilter.addNotEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addNotEquals(binding, value);
		}
*/
	}

	@Override
	public void addNotEquals(String binding, Date value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s notEquals %s", binding, (value == null) ? "null" : value.toString()));
		detailFilter.addNotEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addNotEquals(binding, value);
		}
*/
	}

	@Override
	public void addNotEquals(String binding, Integer value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s notEquals %s", binding, (value == null) ? "null" : value.toString()));
		detailFilter.addNotEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addNotEquals(binding, value);
		}
*/
	}

	@Override
	public void addNotEquals(String binding, Long value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s notEquals %s", binding, (value == null) ? "null" : value.toString()));
		detailFilter.addNotEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addNotEquals(binding, value);
		}
*/
	}

	@Override
	public void addNotEquals(String binding, Decimal value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s notEquals %s", binding, (value == null) ? "null" : value.toString()));
		detailFilter.addNotEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addNotEquals(binding, value);
		}
*/
	}

	@Override
	public void addNotEquals(String binding, Boolean value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s notEquals %s", binding, (value == null) ? "null" : value.toString()));
		detailFilter.addNotEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addNotEquals(binding, value);
		}
*/
	}

	@Override
	public void addNotEquals(String binding, Enum<?> value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s notEquals %s", binding, (value == null) ? "null" : value.toString()));
		detailFilter.addNotEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addNotEquals(binding, value);
		}
*/
	}

	@Override
	public void addNotEquals(String binding, Geometry value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s notEquals %s", binding, (value == null) ? "null" : value.toString()));
		detailFilter.addNotEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addNotEquals(binding, value);
		}
*/
	}

	@Override
	public void addEqualsIgnoreCase(String binding, String value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s equalsIgnoreCase %s", binding, (value == null) ? "null" : value));
		detailFilter.addLike(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLike(binding, value);
		}
*/
	}

	@Override
	public void addNotEqualsIgnoreCase(String binding, String value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s notEqualsIgnoreCase %s", binding, (value == null) ? "null" : value));
		detailFilter.addNotLike(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addNotLike(binding, value);
		}
*/
	}

	@Override
	public void addContains(String binding, String value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s contains %s", binding, (value == null) ? "null" : value));
		String operand = new StringBuilder(32).append('%').append(value).append('%').toString();
		detailFilter.addLike(binding, operand);
		if (summaryFilter != null) {
			summaryFilter.addLike(binding, operand);
		}
*/
	}

	@Override
	public void addNotContains(String binding, String value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s notContains %s", binding, (value == null) ? "null" : value));
		String operand = new StringBuilder(32).append('%').append(value).append('%').toString();
		detailFilter.addNullOrNotLike(binding, operand);
		if (summaryFilter != null) {
			summaryFilter.addNullOrNotLike(binding, operand);
		}
*/
	}

	@Override
	public void addStartsWith(String binding, String value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s startsWith %s", binding, (value == null) ? "null" : value));
		String operand = new StringBuilder(32).append(value).append('%').toString();
		detailFilter.addLike(binding, operand);
		if (summaryFilter != null) {
			summaryFilter.addLike(binding, operand);
		}
*/
	}

	@Override
	public void addNotStartsWith(String binding, String value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s notStartsWith %s", binding, (value == null) ? "null" : value));
		String operand = new StringBuilder(32).append(value).append('%').toString();
		detailFilter.addNullOrNotLike(binding, operand);
		if (summaryFilter != null) {
			summaryFilter.addNullOrNotLike(binding, operand);
		}
*/
	}

	@Override
	public void addEndsWith(String binding, String value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s endsWith %s", binding, (value == null) ? "null" : value));
		String operand = new StringBuilder(32).append('%').append(value).toString();
		detailFilter.addLike(binding, operand);
		if (summaryFilter != null) {
			summaryFilter.addLike(binding, operand);
		}
*/
	}

	@Override
	public void addNotEndsWith(String binding, String value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s notEndsWith %s", binding, (value == null) ? "null" : value));
		String operand = new StringBuilder(32).append('%').append(value).toString();
		detailFilter.addNullOrNotLike(binding, operand);
		if (summaryFilter != null) {
			summaryFilter.addNullOrNotLike(binding, operand);
		}
*/
	}

	@Override
	public void addGreaterThan(String binding, String value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s greaterThan %s", binding, (value == null) ? "null" : value));
		detailFilter.addGreaterThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThan(binding, value);
		}
*/
	}

	@Override
	public void addGreaterThan(String binding, Date value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s greaterThan %s", binding, (value == null) ? "null" : value.toString()));
		detailFilter.addGreaterThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThan(binding, value);
		}
*/
	}

	@Override
	public void addGreaterThan(String binding, Integer value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s greaterThan %s", binding, (value == null) ? "null" : value.toString()));
		detailFilter.addGreaterThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThan(binding, value);
		}
*/
	}

	@Override
	public void addGreaterThan(String binding, Long value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s greaterThan %s", binding, (value == null) ? "null" : value.toString()));
		detailFilter.addGreaterThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThan(binding, value);
		}
*/
	}

	@Override
	public void addGreaterThan(String binding, Decimal value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s greaterThan %s", binding, (value == null) ? "null" : value.toString()));
		detailFilter.addGreaterThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThan(binding, value);
		}
*/
	}

	@Override
	public void addGreaterThanOrEqualTo(String binding, String value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s greaterThanOrEqualTo %s", binding, (value == null) ? "null" : value));
		detailFilter.addGreaterThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThanOrEqualTo(binding, value);
		}
*/
	}

	@Override
	public void addGreaterThanOrEqualTo(String binding, Date value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s greaterThanOrEqualTo %s", binding, (value == null) ? "null" : value.toString()));
		detailFilter.addGreaterThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThanOrEqualTo(binding, value);
		}
*/
	}

	@Override
	public void addGreaterThanOrEqualTo(String binding, Integer value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s greaterThanOrEqualTo %s", binding, (value == null) ? "null" : value.toString()));
		detailFilter.addGreaterThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThanOrEqualTo(binding, value);
		}
*/
	}

	@Override
	public void addGreaterThanOrEqualTo(String binding, Long value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s greaterThanOrEqualTo %s", binding, (value == null) ? "null" : value.toString()));
		detailFilter.addGreaterThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThanOrEqualTo(binding, value);
		}
*/
	}

	@Override
	public void addGreaterThanOrEqualTo(String binding, Decimal value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s greaterThanOrEqualTo %s", binding, (value == null) ? "null" : value.toString()));
		detailFilter.addGreaterThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThanOrEqualTo(binding, value);
		}
*/
	}

	@Override
	public void addLessThan(String binding, String value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s lessThan %s", binding, (value == null) ? "null" : value));
		detailFilter.addLessThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThan(binding, value);
		}
*/
	}

	@Override
	public void addLessThan(String binding, Date value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s lessThan %s", binding, (value == null) ? "null" : value.toString()));
		detailFilter.addLessThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThan(binding, value);
		}
*/
	}

	@Override
	public void addLessThan(String binding, Integer value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s lessThan %s", binding, (value == null) ? "null" : value.toString()));
		detailFilter.addLessThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThan(binding, value);
		}
*/
	}

	@Override
	public void addLessThan(String binding, Long value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s lessThan %s", binding, (value == null) ? "null" : value.toString()));
		detailFilter.addLessThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThan(binding, value);
		}
*/
	}

	@Override
	public void addLessThan(String binding, Decimal value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s lessThan %s", binding, (value == null) ? "null" : value.toString()));
		detailFilter.addLessThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThan(binding, value);
		}
*/
	}

	@Override
	public void addLessThanOrEqualTo(String binding, String value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s lessThanOrEqualTo %s", binding, (value == null) ? "null" : value));
		detailFilter.addLessThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThanOrEqualTo(binding, value);
		}
*/
	}

	@Override
	public void addLessThanOrEqualTo(String binding, Date value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s lessThanOrEqualTo %s", binding, (value == null) ? "null" : value.toString()));
		detailFilter.addLessThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThanOrEqualTo(binding, value);
		}
*/
	}

	@Override
	public void addLessThanOrEqualTo(String binding, Integer value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s lessThanOrEqualTo %s", binding, (value == null) ? "null" : value.toString()));
		detailFilter.addLessThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThanOrEqualTo(binding, value);
		}
*/
	}

	@Override
	public void addLessThanOrEqualTo(String binding, Long value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s lessThanOrEqualTo %s", binding, (value == null) ? "null" : value.toString()));
		detailFilter.addLessThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThanOrEqualTo(binding, value);
		}
*/
	}

	@Override
	public void addLessThanOrEqualTo(String binding, Decimal value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s lessThanOrEqualTo %s", binding, (value == null) ? "null" : value.toString()));
		detailFilter.addLessThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThanOrEqualTo(binding, value);
		}
*/
	}

	@Override
	public void addBetween(String binding, String start, String end) {
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
	public void addBetween(String binding, Date start, Date end) {
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
	public void addBetween(String binding, Integer start, Integer end) {
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
	public void addBetween(String binding, Long start, Long end) {
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
	public void addBetween(String binding, Decimal start, Decimal end) {
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
	public void addWithin(String binding, Geometry value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s within %s", binding, (value == null) ? "null" : value.toString()));
		detailFilter.addWithin(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addWithin(binding, value);
		}
*/
	}

	@Override
	public void addContains(String binding, Geometry value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s contains %s", binding, (value == null) ? "null" : value.toString()));
		detailFilter.addContains(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addContains(binding, value);
		}
*/
	}

	@Override
	public void addCrosses(String binding, Geometry value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s crosses %s", binding, (value == null) ? "null" : value.toString()));
		detailFilter.addCrosses(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addCrosses(binding, value);
		}
*/
	}

	@Override
	public void addDisjoint(String binding, Geometry value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s disjoint %s", binding, (value == null) ? "null" : value.toString()));
		detailFilter.addDisjoint(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addDisjoint(binding, value);
		}
*/
	}

	@Override
	public void addIntersects(String binding, Geometry value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s intersects %s", binding, (value == null) ? "null" : value.toString()));
		detailFilter.addIntersects(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addIntersects(binding, value);
		}
*/
	}

	@Override
	public void addOverlaps(String binding, Geometry value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s overlaps %s", binding, (value == null) ? "null" : value.toString()));
		detailFilter.addOverlaps(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addOverlaps(binding, value);
		}
*/
	}

	@Override
	public void addTouches(String binding, Geometry value) {
		empty = false;
/*
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(String.format("%s touches %s", binding, (value == null) ? "null" : value.toString()));
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
