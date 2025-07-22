package org.skyve.metadata.view.model.list;

import java.util.Date;

import org.locationtech.jts.geom.Geometry;
import org.skyve.CORE;
import org.skyve.domain.types.Decimal;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentFilter;
import org.skyve.util.logging.Category;
import org.slf4j.Logger;

public class DocumentQueryFilter implements Filter {
    private static final Logger QUERY_LOGGER = Category.QUERY.logger();
	private static final String EQUALS_LOG = "{} equals {}";
	private static final String NOT_EQUALS_LOG = "{} notEquals {}";
	private static final String GREATER_THAN_LOG = "{} greaterThan {}";
	private static final String GREATER_THAN_EQUAL_LOG = "{} greaterThanOrEqualTo {}";
	private static final String LESS_THAN_LOG = "{} lessThan {}";
	private static final String LESS_THAN_EQUAL_LOG = "{} lessThanOrEqualTo {}";
	private static final String BETWEEN_LOG = "{} between {} and {}";
	
	private DocumentFilter detailFilter;
	private DocumentFilter summaryFilter;
	private User user;
	// Indicates if the filter is empty - ie has no predicates added
	private boolean empty = true;
	
	DocumentQueryFilter(DocumentFilter detailFilter, DocumentFilter summaryFilter) {
		this.detailFilter = detailFilter;
		this.summaryFilter = summaryFilter;
		user = CORE.getUser();
	}
	
	@Override
	public void addAnd(Filter filter) {
		empty = false;
		detailFilter.addAnd(((DocumentQueryFilter) filter).detailFilter);
		if (summaryFilter != null) {
			summaryFilter.addAnd(((DocumentQueryFilter) filter).summaryFilter);
		}
	}

	@Override
	public void addOr(Filter filter) {
		empty = false;
		detailFilter.addOr(((DocumentQueryFilter) filter).detailFilter);
		if (summaryFilter != null) {
			summaryFilter.addOr(((DocumentQueryFilter) filter).summaryFilter);
		}
	}

	@Override
	public void addTagged(String tagId, boolean tagged) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("add tagged {} with tagId {}", Boolean.valueOf(tagged), tagId);
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
	}

	@Override
	public void addNull(String binding) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("{} is null", binding);
		detailFilter.addNull(binding);
		if (summaryFilter != null) {
			summaryFilter.addNull(binding);
		}
	}

	@Override
	public void addNotNull(String binding) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("{} is not null", binding);
		detailFilter.addNotNull(binding);
		if (summaryFilter != null) {
			summaryFilter.addNotNull(binding);
		}
	}

	@Override
	public void addEquals(String binding, String value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(EQUALS_LOG, binding, value);
		detailFilter.addEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addEquals(binding, value);
		}
	}

	@Override
	public void addEquals(String binding, Date value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(EQUALS_LOG, binding, value);
		detailFilter.addEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addEquals(binding, value);
		}
	}

	@Override
	public void addEquals(String binding, Integer value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(EQUALS_LOG, binding, value);
		detailFilter.addEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addEquals(binding, value);
		}
	}

	@Override
	public void addEquals(String binding, Long value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(EQUALS_LOG, binding, value);
		detailFilter.addEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addEquals(binding, value);
		}
	}

	@Override
	public void addEquals(String binding, Decimal value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(EQUALS_LOG, binding, value);
		detailFilter.addEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addEquals(binding, value);
		}
	}

	@Override
	public void addEquals(String binding, Boolean value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(EQUALS_LOG, binding, value);
		detailFilter.addEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addEquals(binding, value);
		}
	}

	@Override
	public void addEquals(String binding, Enum<?> value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(EQUALS_LOG, binding, value);
		detailFilter.addEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addEquals(binding, value);
		}	
	}

	@Override
	public void addEquals(String binding, Geometry value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(EQUALS_LOG, binding, value);
		detailFilter.addEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addEquals(binding, value);
		}
	}

	@Override
	public void addNotEquals(String binding, String value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(NOT_EQUALS_LOG, binding, value);
		detailFilter.addNotEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addNotEquals(binding, value);
		}
	}

	@Override
	public void addNotEquals(String binding, Date value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(NOT_EQUALS_LOG, binding, value);
		detailFilter.addNotEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addNotEquals(binding, value);
		}
	}

	@Override
	public void addNotEquals(String binding, Integer value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(NOT_EQUALS_LOG, binding, value);
		detailFilter.addNotEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addNotEquals(binding, value);
		}
	}

	@Override
	public void addNotEquals(String binding, Long value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(NOT_EQUALS_LOG, binding, value);
		detailFilter.addNotEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addNotEquals(binding, value);
		}
	}

	@Override
	public void addNotEquals(String binding, Decimal value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(NOT_EQUALS_LOG, binding, value);
		detailFilter.addNotEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addNotEquals(binding, value);
		}
	}

	@Override
	public void addNotEquals(String binding, Boolean value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(NOT_EQUALS_LOG, binding, value);
		detailFilter.addNotEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addNotEquals(binding, value);
		}
	}

	@Override
	public void addNotEquals(String binding, Enum<?> value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(NOT_EQUALS_LOG, binding, value);
		detailFilter.addNotEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addNotEquals(binding, value);
		}
	}

	@Override
	public void addNotEquals(String binding, Geometry value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(NOT_EQUALS_LOG, binding, value);
		detailFilter.addNotEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addNotEquals(binding, value);
		}
	}

	@Override
	public void addEqualsIgnoreCase(String binding, String value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("{} equalsIgnoreCase {}", binding, value);
		detailFilter.addLike(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLike(binding, value);
		}
	}

	@Override
	public void addNotEqualsIgnoreCase(String binding, String value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("{} notEqualsIgnoreCase {}", binding, value);
		detailFilter.addNotLike(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addNotLike(binding, value);
		}
	}

	@Override
	public void addContains(String binding, String value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("{} contains {}", binding, value);
		String operand = new StringBuilder(32).append('%').append(value).append('%').toString();
		detailFilter.addLike(binding, operand);
		if (summaryFilter != null) {
			summaryFilter.addLike(binding, operand);
		}
	}

	@Override
	public void addNotContains(String binding, String value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("{} notContains {}", binding, value);
		String operand = new StringBuilder(32).append('%').append(value).append('%').toString();
		detailFilter.addNullOrNotLike(binding, operand);
		if (summaryFilter != null) {
			summaryFilter.addNullOrNotLike(binding, operand);
		}
	}

	@Override
	public void addStartsWith(String binding, String value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("{} startsWith {}", binding, value);
		String operand = new StringBuilder(32).append(value).append('%').toString();
		detailFilter.addLike(binding, operand);
		if (summaryFilter != null) {
			summaryFilter.addLike(binding, operand);
		}
	}

	@Override
	public void addNotStartsWith(String binding, String value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("{} notStartsWith {}", binding, value);
		String operand = new StringBuilder(32).append(value).append('%').toString();
		detailFilter.addNullOrNotLike(binding, operand);
		if (summaryFilter != null) {
			summaryFilter.addNullOrNotLike(binding, operand);
		}
	}

	@Override
	public void addEndsWith(String binding, String value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("{} endsWith {}", binding, value);
		String operand = new StringBuilder(32).append('%').append(value).toString();
		detailFilter.addLike(binding, operand);
		if (summaryFilter != null) {
			summaryFilter.addLike(binding, operand);
		}
	}

	@Override
	public void addNotEndsWith(String binding, String value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("{} notEndsWith {}", binding, value);
		String operand = new StringBuilder(32).append('%').append(value).toString();
		detailFilter.addNullOrNotLike(binding, operand);
		if (summaryFilter != null) {
			summaryFilter.addNullOrNotLike(binding, operand);
		}
	}

	@Override
	public void addGreaterThan(String binding, String value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(GREATER_THAN_LOG, binding, value);
		detailFilter.addGreaterThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThan(binding, value);
		}
	}

	@Override
	public void addGreaterThan(String binding, Date value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(GREATER_THAN_LOG, binding, value);
		detailFilter.addGreaterThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThan(binding, value);
		}
	}

	@Override
	public void addGreaterThan(String binding, Integer value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(GREATER_THAN_LOG, binding, value);
		detailFilter.addGreaterThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThan(binding, value);
		}
	}

	@Override
	public void addGreaterThan(String binding, Long value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(GREATER_THAN_LOG, binding, value);
		detailFilter.addGreaterThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThan(binding, value);
		}
	}

	@Override
	public void addGreaterThan(String binding, Decimal value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(GREATER_THAN_LOG, binding, value);
		detailFilter.addGreaterThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThan(binding, value);
		}
	}

	@Override
	public void addGreaterThanOrEqualTo(String binding, String value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(GREATER_THAN_EQUAL_LOG, binding, value);
		detailFilter.addGreaterThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThanOrEqualTo(binding, value);
		}
	}

	@Override
	public void addGreaterThanOrEqualTo(String binding, Date value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(GREATER_THAN_EQUAL_LOG, binding, value);
		detailFilter.addGreaterThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThanOrEqualTo(binding, value);
		}
	}

	@Override
	public void addGreaterThanOrEqualTo(String binding, Integer value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(GREATER_THAN_EQUAL_LOG, binding, value);
		detailFilter.addGreaterThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThanOrEqualTo(binding, value);
		}
	}

	@Override
	public void addGreaterThanOrEqualTo(String binding, Long value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(GREATER_THAN_EQUAL_LOG, binding, value);
		detailFilter.addGreaterThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThanOrEqualTo(binding, value);
		}
	}

	@Override
	public void addGreaterThanOrEqualTo(String binding, Decimal value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(GREATER_THAN_EQUAL_LOG, binding, value);
		detailFilter.addGreaterThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThanOrEqualTo(binding, value);
		}
	}

	@Override
	public void addLessThan(String binding, String value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(LESS_THAN_LOG, binding, value);
		detailFilter.addLessThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThan(binding, value);
		}
	}

	@Override
	public void addLessThan(String binding, Date value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(LESS_THAN_LOG, binding, value);
		detailFilter.addLessThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThan(binding, value);
		}
	}

	@Override
	public void addLessThan(String binding, Integer value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(LESS_THAN_LOG, binding, value);
		detailFilter.addLessThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThan(binding, value);
		}
	}

	@Override
	public void addLessThan(String binding, Long value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(LESS_THAN_LOG, binding, value);
		detailFilter.addLessThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThan(binding, value);
		}
	}

	@Override
	public void addLessThan(String binding, Decimal value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(LESS_THAN_LOG, binding, value);
		detailFilter.addLessThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThan(binding, value);
		}
	}

	@Override
	public void addLessThanOrEqualTo(String binding, String value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(LESS_THAN_EQUAL_LOG, binding, value);
		detailFilter.addLessThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThanOrEqualTo(binding, value);
		}
	}

	@Override
	public void addLessThanOrEqualTo(String binding, Date value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(LESS_THAN_EQUAL_LOG, binding, value);
		detailFilter.addLessThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThanOrEqualTo(binding, value);
		}
	}

	@Override
	public void addLessThanOrEqualTo(String binding, Integer value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(LESS_THAN_EQUAL_LOG, binding, value);
		detailFilter.addLessThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThanOrEqualTo(binding, value);
		}
	}

	@Override
	public void addLessThanOrEqualTo(String binding, Long value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(LESS_THAN_EQUAL_LOG, binding, value);
		detailFilter.addLessThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThanOrEqualTo(binding, value);
		}
	}

	@Override
	public void addLessThanOrEqualTo(String binding, Decimal value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(LESS_THAN_EQUAL_LOG, binding, value);
		detailFilter.addLessThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThanOrEqualTo(binding, value);
		}
	}

	@Override
	public void addBetween(String binding, String start, String end) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(BETWEEN_LOG, binding, start, end);
		detailFilter.addBetween(binding, start, end);
		if (summaryFilter != null) {
			summaryFilter.addBetween(binding, start, end);
		}
	}

	@Override
	public void addBetween(String binding, Date start, Date end) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(BETWEEN_LOG, binding, start, end);
		detailFilter.addBetween(binding, start, end);
		if (summaryFilter != null) {
			summaryFilter.addBetween(binding, start, end);
		}
	}

	@Override
	public void addBetween(String binding, Integer start, Integer end) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(BETWEEN_LOG, binding, start, end);
		detailFilter.addBetween(binding, start, end);
		if (summaryFilter != null) {
			summaryFilter.addBetween(binding, start, end);
		}
	}

	@Override
	public void addBetween(String binding, Long start, Long end) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(BETWEEN_LOG, binding, start, end);
		detailFilter.addBetween(binding, start, end);
		if (summaryFilter != null) {
			summaryFilter.addBetween(binding, start, end);
		}
	}

	@Override
	public void addBetween(String binding, Decimal start, Decimal end) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(BETWEEN_LOG, binding, start, end);
		detailFilter.addBetween(binding, start, end);
		if (summaryFilter != null) {
			summaryFilter.addBetween(binding, start, end);
		}
	}

	@Override
	public void addIn(String binding, Object... values) {
		in(binding, false, values);
	}

	@Override
	public void addNotIn(String binding, Object... values) {
		in(binding, true, values);
	}
	
	private void in(String binding, boolean not, Object... values) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) {
			StringBuilder sb = new StringBuilder(20 + (7 * values.length));
			sb.append(binding).append(not ? " not in " : " in ");
			for (Object value : values) {
				sb.append(value).append(", ");
			}
			sb.setLength(sb.length() - 2);
			QUERY_LOGGER.info(sb.toString());
		}
		if (values.length > 0) {
			if (not) {
				detailFilter.addNotIn(binding, values);
			}
			else {
				detailFilter.addIn(binding, values);
			}
		}
		else {
			// select nothing
			detailFilter.addNull(binding);
			detailFilter.addNotNull(binding);
		}
		if (summaryFilter != null) {
			if (values.length > 0) {
				if (not) {
					summaryFilter.addNotIn(binding, values);
				}
				else {
					summaryFilter.addIn(binding, values);
				}
			}
			else {
				// select nothing
				summaryFilter.addNull(binding);
				summaryFilter.addNotNull(binding);
			}
		}
	}
	
	@Override
	public void addWithin(String binding, Geometry value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("{} within {}", binding, value);
		detailFilter.addWithin(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addWithin(binding, value);
		}
	}

	@Override
	public void addContains(String binding, Geometry value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("{} contains {}", binding, value);
		detailFilter.addContains(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addContains(binding, value);
		}
	}

	@Override
	public void addCrosses(String binding, Geometry value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("{} crosses {}", binding, value);
		detailFilter.addCrosses(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addCrosses(binding, value);
		}
	}

	@Override
	public void addDisjoint(String binding, Geometry value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("{} disjoint {}", binding, value);
		detailFilter.addDisjoint(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addDisjoint(binding, value);
		}
	}

	@Override
	public void addIntersects(String binding, Geometry value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("{} intersects {}", binding, value);
		detailFilter.addIntersects(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addIntersects(binding, value);
		}
	}

	@Override
	public void addOverlaps(String binding, Geometry value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("{} overlaps {}", binding, value);
		detailFilter.addOverlaps(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addOverlaps(binding, value);
		}
	}

	@Override
	public void addTouches(String binding, Geometry value) {
		empty = false;
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("{} touches {}", binding, value);
		detailFilter.addTouches(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addTouches(binding, value);
		}
	}

	@Override
	public boolean isEmpty() {
		return empty;
	}
}
