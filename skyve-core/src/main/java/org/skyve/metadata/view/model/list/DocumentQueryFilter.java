package org.skyve.metadata.view.model.list;

import java.util.Date;
import java.util.function.Consumer;

import org.locationtech.jts.geom.Geometry;
import org.skyve.CORE;
import org.skyve.domain.types.Decimal;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentFilter;
import org.skyve.util.logging.Category;
import org.slf4j.Logger;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

public class DocumentQueryFilter implements Filter {
    private static final Logger QUERY_LOGGER = Category.QUERY.logger();
	private static final String EQUALS_LOG = "{} equals {}";
	private static final String NOT_EQUALS_LOG = "{} notEquals {}";
	private static final String GREATER_THAN_LOG = "{} greaterThan {}";
	private static final String GREATER_THAN_EQUAL_LOG = "{} greaterThanOrEqualTo {}";
	private static final String LESS_THAN_LOG = "{} lessThan {}";
	private static final String LESS_THAN_EQUAL_LOG = "{} lessThanOrEqualTo {}";
	private static final String BETWEEN_LOG = "{} between {} and {}";
	// A predicate that holds for every row: "not in" an empty set (it still counts when ORed with another predicate)
	private static final String EVERY_ROW = "1 = 1";
	
	private DocumentFilter detailFilter;
	private DocumentFilter summaryFilter;
	private User user;
	// Indicates if the filter is empty - ie has no predicates added
	private boolean empty = true;
	
	// told of each binding a condition is put on, so that the query can keep the rows with no association on its path
	private final Consumer<String> filteredBindings;

	/**
	 * Creates a filter that puts each condition on both document filters.
	 * @param detailFilter	the filter of the list's rows
	 * @param summaryFilter	the filter of its summary (count) query, given the same conditions, if there is one
	 * @param filteredBindings	told of the binding of each condition added (not of tags, and not of the filters an
	 * 							and or an or adds, which told it of their own)
	 */
	DocumentQueryFilter(@Nonnull DocumentFilter detailFilter,
							@Nullable DocumentFilter summaryFilter,
							@Nonnull Consumer<String> filteredBindings) {
		this.detailFilter = detailFilter;
		this.summaryFilter = summaryFilter;
		this.filteredBindings = filteredBindings;
		user = CORE.getUser();
	}

	/**
	 * Adds a condition on a binding to the detail and the summary filter.
	 * <p>Side effects: tells filteredBindings of the binding first, and marks this filter not empty.
	 * @param binding	the binding the condition is on
	 * @param condition	adds the condition to the document filter given
	 */
	private void add(@Nonnull String binding, @Nonnull Consumer<DocumentFilter> condition) {
		empty = false;
		filteredBindings.accept(binding);
		condition.accept(detailFilter);
		if (summaryFilter != null) {
			condition.accept(summaryFilter);
		}
	}
	
	@Override
	public void addAnd(@Nonnull Filter filter) {
		empty = false;
		detailFilter.addAnd(((DocumentQueryFilter) filter).detailFilter);
		if (summaryFilter != null) {
			summaryFilter.addAnd(((DocumentQueryFilter) filter).summaryFilter);
		}
	}

	@Override
	public void addOr(@Nonnull Filter filter) {
		empty = false;
		detailFilter.addOr(((DocumentQueryFilter) filter).detailFilter);
		if (summaryFilter != null) {
			summaryFilter.addOr(((DocumentQueryFilter) filter).summaryFilter);
		}
	}

	@Override
	public void addTagged(@Nonnull String tagId, boolean tagged) {
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
	public void addNull(@Nonnull String binding) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("{} is null", binding);
		add(binding, f -> f.addNull(binding));
	}

	@Override
	public void addNotNull(@Nonnull String binding) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("{} is not null", binding);
		add(binding, f -> f.addNotNull(binding));
	}

	@Override
	public void addEquals(@Nonnull String binding, @Nonnull String value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(EQUALS_LOG, binding, value);
		add(binding, f -> f.addEquals(binding, value));
	}

	@Override
	public void addEquals(@Nonnull String binding, @Nonnull Date value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(EQUALS_LOG, binding, value);
		add(binding, f -> f.addEquals(binding, value));
	}

	@Override
	public void addEquals(@Nonnull String binding, @Nonnull Integer value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(EQUALS_LOG, binding, value);
		add(binding, f -> f.addEquals(binding, value));
	}

	@Override
	public void addEquals(@Nonnull String binding, @Nonnull Long value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(EQUALS_LOG, binding, value);
		add(binding, f -> f.addEquals(binding, value));
	}

	@Override
	public void addEquals(@Nonnull String binding, @Nonnull Decimal value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(EQUALS_LOG, binding, value);
		add(binding, f -> f.addEquals(binding, value));
	}

	@Override
	public void addEquals(@Nonnull String binding, @Nonnull Boolean value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(EQUALS_LOG, binding, value);
		add(binding, f -> f.addEquals(binding, value));
	}

	@Override
	public void addEquals(@Nonnull String binding, @Nonnull Enum<?> value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(EQUALS_LOG, binding, value);
		add(binding, f -> f.addEquals(binding, value));
	}

	@Override
	public void addEquals(@Nonnull String binding, @Nonnull Geometry value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(EQUALS_LOG, binding, value);
		add(binding, f -> f.addEquals(binding, value));
	}

	@Override
	public void addNotEquals(@Nonnull String binding, @Nonnull String value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(NOT_EQUALS_LOG, binding, value);
		add(binding, f -> f.addNotEquals(binding, value));
	}

	@Override
	public void addNotEquals(@Nonnull String binding, @Nonnull Date value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(NOT_EQUALS_LOG, binding, value);
		add(binding, f -> f.addNotEquals(binding, value));
	}

	@Override
	public void addNotEquals(@Nonnull String binding, @Nonnull Integer value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(NOT_EQUALS_LOG, binding, value);
		add(binding, f -> f.addNotEquals(binding, value));
	}

	@Override
	public void addNotEquals(@Nonnull String binding, @Nonnull Long value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(NOT_EQUALS_LOG, binding, value);
		add(binding, f -> f.addNotEquals(binding, value));
	}

	@Override
	public void addNotEquals(@Nonnull String binding, @Nonnull Decimal value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(NOT_EQUALS_LOG, binding, value);
		add(binding, f -> f.addNotEquals(binding, value));
	}

	@Override
	public void addNotEquals(@Nonnull String binding, @Nonnull Boolean value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(NOT_EQUALS_LOG, binding, value);
		add(binding, f -> f.addNotEquals(binding, value));
	}

	@Override
	public void addNotEquals(@Nonnull String binding, @Nonnull Enum<?> value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(NOT_EQUALS_LOG, binding, value);
		add(binding, f -> f.addNotEquals(binding, value));
	}

	@Override
	public void addNotEquals(@Nonnull String binding, @Nonnull Geometry value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(NOT_EQUALS_LOG, binding, value);
		add(binding, f -> f.addNotEquals(binding, value));
	}

	@Override
	public void addEqualsIgnoreCase(@Nonnull String binding, @Nonnull String value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("{} equalsIgnoreCase {}", binding, value);
		add(binding, f -> f.addLike(binding, value));
	}

	@Override
	public void addNotEqualsIgnoreCase(@Nonnull String binding, @Nonnull String value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("{} notEqualsIgnoreCase {}", binding, value);
		add(binding, f -> f.addNotLike(binding, value));
	}

	@Override
	public void addContains(@Nonnull String binding, @Nonnull String value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("{} contains {}", binding, value);
		String operand = new StringBuilder(32).append('%').append(value).append('%').toString();
		add(binding, f -> f.addLike(binding, operand));
	}

	@Override
	public void addNotContains(@Nonnull String binding, @Nonnull String value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("{} notContains {}", binding, value);
		String operand = new StringBuilder(32).append('%').append(value).append('%').toString();
		add(binding, f -> f.addNullOrNotLike(binding, operand));
	}

	@Override
	public void addStartsWith(@Nonnull String binding, @Nonnull String value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("{} startsWith {}", binding, value);
		String operand = new StringBuilder(32).append(value).append('%').toString();
		add(binding, f -> f.addLike(binding, operand));
	}

	@Override
	public void addNotStartsWith(@Nonnull String binding, @Nonnull String value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("{} notStartsWith {}", binding, value);
		String operand = new StringBuilder(32).append(value).append('%').toString();
		add(binding, f -> f.addNullOrNotLike(binding, operand));
	}

	@Override
	public void addEndsWith(@Nonnull String binding, @Nonnull String value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("{} endsWith {}", binding, value);
		String operand = new StringBuilder(32).append('%').append(value).toString();
		add(binding, f -> f.addLike(binding, operand));
	}

	@Override
	public void addNotEndsWith(@Nonnull String binding, @Nonnull String value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("{} notEndsWith {}", binding, value);
		String operand = new StringBuilder(32).append('%').append(value).toString();
		add(binding, f -> f.addNullOrNotLike(binding, operand));
	}

	@Override
	public void addGreaterThan(@Nonnull String binding, @Nonnull String value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(GREATER_THAN_LOG, binding, value);
		add(binding, f -> f.addGreaterThan(binding, value));
	}

	@Override
	public void addGreaterThan(@Nonnull String binding, @Nonnull Date value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(GREATER_THAN_LOG, binding, value);
		add(binding, f -> f.addGreaterThan(binding, value));
	}

	@Override
	public void addGreaterThan(@Nonnull String binding, @Nonnull Integer value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(GREATER_THAN_LOG, binding, value);
		add(binding, f -> f.addGreaterThan(binding, value));
	}

	@Override
	public void addGreaterThan(@Nonnull String binding, @Nonnull Long value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(GREATER_THAN_LOG, binding, value);
		add(binding, f -> f.addGreaterThan(binding, value));
	}

	@Override
	public void addGreaterThan(@Nonnull String binding, @Nonnull Decimal value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(GREATER_THAN_LOG, binding, value);
		add(binding, f -> f.addGreaterThan(binding, value));
	}

	@Override
	public void addGreaterThanOrEqualTo(@Nonnull String binding, @Nonnull String value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(GREATER_THAN_EQUAL_LOG, binding, value);
		add(binding, f -> f.addGreaterThanOrEqualTo(binding, value));
	}

	@Override
	public void addGreaterThanOrEqualTo(@Nonnull String binding, @Nonnull Date value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(GREATER_THAN_EQUAL_LOG, binding, value);
		add(binding, f -> f.addGreaterThanOrEqualTo(binding, value));
	}

	@Override
	public void addGreaterThanOrEqualTo(@Nonnull String binding, @Nonnull Integer value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(GREATER_THAN_EQUAL_LOG, binding, value);
		add(binding, f -> f.addGreaterThanOrEqualTo(binding, value));
	}

	@Override
	public void addGreaterThanOrEqualTo(@Nonnull String binding, @Nonnull Long value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(GREATER_THAN_EQUAL_LOG, binding, value);
		add(binding, f -> f.addGreaterThanOrEqualTo(binding, value));
	}

	@Override
	public void addGreaterThanOrEqualTo(@Nonnull String binding, @Nonnull Decimal value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(GREATER_THAN_EQUAL_LOG, binding, value);
		add(binding, f -> f.addGreaterThanOrEqualTo(binding, value));
	}

	@Override
	public void addLessThan(@Nonnull String binding, @Nonnull String value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(LESS_THAN_LOG, binding, value);
		add(binding, f -> f.addLessThan(binding, value));
	}

	@Override
	public void addLessThan(@Nonnull String binding, @Nonnull Date value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(LESS_THAN_LOG, binding, value);
		add(binding, f -> f.addLessThan(binding, value));
	}

	@Override
	public void addLessThan(@Nonnull String binding, @Nonnull Integer value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(LESS_THAN_LOG, binding, value);
		add(binding, f -> f.addLessThan(binding, value));
	}

	@Override
	public void addLessThan(@Nonnull String binding, @Nonnull Long value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(LESS_THAN_LOG, binding, value);
		add(binding, f -> f.addLessThan(binding, value));
	}

	@Override
	public void addLessThan(@Nonnull String binding, @Nonnull Decimal value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(LESS_THAN_LOG, binding, value);
		add(binding, f -> f.addLessThan(binding, value));
	}

	@Override
	public void addLessThanOrEqualTo(@Nonnull String binding, @Nonnull String value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(LESS_THAN_EQUAL_LOG, binding, value);
		add(binding, f -> f.addLessThanOrEqualTo(binding, value));
	}

	@Override
	public void addLessThanOrEqualTo(@Nonnull String binding, @Nonnull Date value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(LESS_THAN_EQUAL_LOG, binding, value);
		add(binding, f -> f.addLessThanOrEqualTo(binding, value));
	}

	@Override
	public void addLessThanOrEqualTo(@Nonnull String binding, @Nonnull Integer value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(LESS_THAN_EQUAL_LOG, binding, value);
		add(binding, f -> f.addLessThanOrEqualTo(binding, value));
	}

	@Override
	public void addLessThanOrEqualTo(@Nonnull String binding, @Nonnull Long value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(LESS_THAN_EQUAL_LOG, binding, value);
		add(binding, f -> f.addLessThanOrEqualTo(binding, value));
	}

	@Override
	public void addLessThanOrEqualTo(@Nonnull String binding, @Nonnull Decimal value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(LESS_THAN_EQUAL_LOG, binding, value);
		add(binding, f -> f.addLessThanOrEqualTo(binding, value));
	}

	@Override
	public void addBetween(@Nonnull String binding, @Nonnull String start, @Nonnull String end) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(BETWEEN_LOG, binding, start, end);
		add(binding, f -> f.addBetween(binding, start, end));
	}

	@Override
	public void addBetween(@Nonnull String binding, @Nonnull Date start, @Nonnull Date end) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(BETWEEN_LOG, binding, start, end);
		add(binding, f -> f.addBetween(binding, start, end));
	}

	@Override
	public void addBetween(@Nonnull String binding, @Nonnull Integer start, @Nonnull Integer end) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(BETWEEN_LOG, binding, start, end);
		add(binding, f -> f.addBetween(binding, start, end));
	}

	@Override
	public void addBetween(@Nonnull String binding, @Nonnull Long start, @Nonnull Long end) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(BETWEEN_LOG, binding, start, end);
		add(binding, f -> f.addBetween(binding, start, end));
	}

	@Override
	public void addBetween(@Nonnull String binding, @Nonnull Decimal start, @Nonnull Decimal end) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info(BETWEEN_LOG, binding, start, end);
		add(binding, f -> f.addBetween(binding, start, end));
	}

	@Override
	public void addIn(@Nonnull String binding, @Nonnull Object... values) {
		in(binding, false, values);
	}

	@Override
	public void addNotIn(@Nonnull String binding, @Nonnull Object... values) {
		in(binding, true, values);
	}
	
	private void in(@Nonnull String binding, boolean not, @Nonnull Object... values) {
		if (UtilImpl.QUERY_TRACE) {
			StringBuilder sb = new StringBuilder(20 + (7 * values.length));
			sb.append(binding).append(not ? " not in " : " in ");
			for (Object value : values) {
				sb.append(value).append(", ");
			}
			sb.setLength(sb.length() - 2);
			QUERY_LOGGER.info(sb.toString());
		}
		add(binding, f -> in(f, binding, not, values));
	}

	/**
	 * Adds an in or not in predicate: in an empty set selects nothing, not in an empty set every row.
	 */
	private static void in(@Nonnull DocumentFilter filter, @Nonnull String binding, boolean not, @Nonnull Object... values) {
		if (values.length > 0) {
			if (not) {
				filter.addNotIn(binding, values);
			}
			else {
				filter.addIn(binding, values);
			}
		}
		else if (not) {
			filter.addExpression(EVERY_ROW);
		}
		else {
			// select nothing
			filter.addNull(binding);
			filter.addNotNull(binding);
		}
	}
	
	@Override
	public void addWithin(@Nonnull String binding, @Nonnull Geometry value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("{} within {}", binding, value);
		add(binding, f -> f.addWithin(binding, value));
	}

	@Override
	public void addContains(@Nonnull String binding, @Nonnull Geometry value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("{} contains {}", binding, value);
		add(binding, f -> f.addContains(binding, value));
	}

	@Override
	public void addCrosses(@Nonnull String binding, @Nonnull Geometry value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("{} crosses {}", binding, value);
		add(binding, f -> f.addCrosses(binding, value));
	}

	@Override
	public void addDisjoint(@Nonnull String binding, @Nonnull Geometry value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("{} disjoint {}", binding, value);
		add(binding, f -> f.addDisjoint(binding, value));
	}

	@Override
	public void addIntersects(@Nonnull String binding, @Nonnull Geometry value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("{} intersects {}", binding, value);
		add(binding, f -> f.addIntersects(binding, value));
	}

	@Override
	public void addOverlaps(@Nonnull String binding, @Nonnull Geometry value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("{} overlaps {}", binding, value);
		add(binding, f -> f.addOverlaps(binding, value));
	}

	@Override
	public void addTouches(@Nonnull String binding, @Nonnull Geometry value) {
		if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("{} touches {}", binding, value);
		add(binding, f -> f.addTouches(binding, value));
	}

	@Override
	public boolean isEmpty() {
		return empty;
	}
}
