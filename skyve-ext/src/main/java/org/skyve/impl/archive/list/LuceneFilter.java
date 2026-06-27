package org.skyve.impl.archive.list;

import static org.apache.commons.lang3.StringUtils.toRootLowerCase;
import static org.apache.lucene.search.BooleanClause.Occur.MUST;
import static org.apache.lucene.search.BooleanClause.Occur.MUST_NOT;
import static org.apache.lucene.search.BooleanClause.Occur.SHOULD;
import static org.apache.lucene.search.WildcardQuery.WILDCARD_CHAR;
import static org.apache.lucene.search.WildcardQuery.WILDCARD_ESCAPE;
import static org.apache.lucene.search.WildcardQuery.WILDCARD_STRING;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.lucene.document.DateTools;
import org.apache.lucene.document.DateTools.Resolution;
import org.apache.lucene.document.DoubleField;
import org.apache.lucene.document.IntField;
import org.apache.lucene.document.LongField;
import org.apache.lucene.index.Term;
import org.apache.lucene.search.BooleanClause;
import org.apache.lucene.search.BooleanQuery;
import org.apache.lucene.search.BooleanQuery.Builder;
import org.apache.lucene.search.FieldExistsQuery;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.TermInSetQuery;
import org.apache.lucene.search.TermQuery;
import org.apache.lucene.search.TermRangeQuery;
import org.apache.lucene.search.WildcardQuery;
import org.apache.lucene.util.BytesRef;
import org.locationtech.jts.geom.Geometry;
import org.skyve.domain.types.Decimal;
import org.skyve.metadata.view.model.list.Filter;

import com.google.common.base.MoreObjects;

/**
 * Builds Lucene boolean queries from Skyve list filter operations.
 */
public class LuceneFilter implements Filter {
    private static final String WILDCARD = "*";
    private static final boolean INCLUDE_LOWER_BOUND = true;
    private static final boolean INCLUDE_UPPER_BOUND = true;

    private List<BooleanClause> clauses = new ArrayList<>();

    /**
     * Compiles the currently accumulated clauses into a Lucene query.
     *
     * @return A boolean query representing all applied filter clauses.
     */
    public Query toQuery() {
        Builder builder = new BooleanQuery.Builder();

        for (BooleanClause clause : clauses) {
            builder.add(clause);
        }

        return builder.build();
    }

    /**
     * Lower case and (lucene) escape the provided string value.
     * 
     * @param value
     * @return
     */
    private static String lower(String value) {
        return toRootLowerCase(value);
    }

    /**
     * Adapated from QueryParserBase.escape(), however we only want to escape special
     * characters for WildcardQuery; as we're not using a QueryParser.
     * 
     * @see org.apache.lucene.search.WildcardQuery.toAutomaton(Term)
     * 
     * @param s
     * @return
     */
    private static String escape(String s) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);

            if (c == WILDCARD_STRING ||
                    c == WILDCARD_CHAR ||
                    c == WILDCARD_ESCAPE) {
                sb.append('\\');
            }
            sb.append(c);
        }
        return sb.toString();
    }

    /**
     * Adds another Lucene-backed filter as a mandatory conjunction.
     *
     * @param filter The filter whose query is ANDed into this filter.
     */
    @Override
    public void addAnd(Filter filter) {
        if (filter instanceof LuceneFilter otherFilter) {
            clauses.add(new BooleanClause(otherFilter.toQuery(), MUST));
        }
    }

    /**
     * Combines this filter with another Lucene-backed filter using disjunction.
     *
     * @param filter The filter whose query is ORed with the current query.
     */
    @Override
    public void addOr(Filter filter) {
        if (filter instanceof LuceneFilter otherFilter) {
            Query copyThisQuery = toQuery();
            clauses.clear();

            clauses.add(new BooleanClause(copyThisQuery, SHOULD));
            clauses.add(new BooleanClause(otherFilter.toQuery(), SHOULD));
        }
    }

    /**
     * Adds the tagged constraint.
     */
    @Override
    public void addTagged(String tagId, boolean tagged) {
        throw new UnsupportedOperationException();
    }

    /**
     * Adds the null constraint.
     */
    @Override
    public void addNull(String binding) {
        Query query = new FieldExistsQuery(binding);
        clauses.add(new BooleanClause(query, MUST_NOT));
    }

    /**
     * Adds the not null constraint.
     */
    @Override
    public void addNotNull(String binding) {
        Query query = new FieldExistsQuery(binding);
        clauses.add(new BooleanClause(query, MUST));
    }

    /**
     * Adds the equals constraint.
     */
    @Override
    public void addEquals(String binding, String value) {
        Query query = new TermQuery(new Term(binding, lower(value)));
        clauses.add(new BooleanClause(query, MUST));
    }

    /**
     * Adds the equals constraint.
     */
    @Override
    public void addEquals(String binding, Date value) {
        String dateStr = formatDate(value);
        Query query = new TermQuery(new Term(binding, dateStr));
        clauses.add(new BooleanClause(query, MUST));
    }

    /**
     * Adds the equals constraint.
     */
    @Override
    public void addEquals(String binding, Integer value) {
        Query eq = IntField.newExactQuery(binding, value);
        clauses.add(new BooleanClause(eq, MUST));
    }

    /**
     * Adds the equals constraint.
     */
    @Override
    public void addEquals(String binding, Long value) {
        Query eq = LongField.newExactQuery(binding, value);
        clauses.add(new BooleanClause(eq, MUST));
    }

    /**
     * Adds the equals constraint.
     */
    @Override
    public void addEquals(String binding, Decimal value) {
        Query eq = DoubleField.newExactQuery(binding, value.doubleValue());
        clauses.add(new BooleanClause(eq, MUST));
    }

    /**
     * Adds the equals constraint.
     */
    @Override
    public void addEquals(String binding, Boolean value) {
        addEquals(binding, String.valueOf(value));
    }

    /**
     * Adds the equals constraint.
     */
    @Override
    public void addEquals(String binding, Enum<?> value) {
        addEquals(binding, String.valueOf(value));
    }

    /**
     * Adds the not equals constraint.
     */
    @Override
    public void addNotEquals(String binding, String value) {
        Query query = new TermQuery(new Term(binding, lower(value)));
        clauses.add(new BooleanClause(query, MUST_NOT));
    }

    /**
     * Adds the not equals constraint.
     */
    @Override
    public void addNotEquals(String binding, Date value) {
        String dateStr = formatDate(value);
        addNotEquals(binding, dateStr);
    }

    /**
     * Adds the not equals constraint.
     */
    @Override
    public void addNotEquals(String binding, Integer value) {
        Query iq = IntField.newExactQuery(binding, value);
        clauses.add(new BooleanClause(iq, MUST_NOT));
    }

    /**
     * Adds the not equals constraint.
     */
    @Override
    public void addNotEquals(String binding, Long value) {
        Query iq = LongField.newExactQuery(binding, value);
        clauses.add(new BooleanClause(iq, MUST_NOT));
    }

    /**
     * Adds the not equals constraint.
     */
    @Override
    public void addNotEquals(String binding, Decimal value) {
        Query eq = DoubleField.newExactQuery(binding, value.doubleValue());
        clauses.add(new BooleanClause(eq, MUST_NOT));
    }

    /**
     * Adds the not equals constraint.
     */
    @Override
    public void addNotEquals(String binding, Boolean value) {
        addNotEquals(binding, String.valueOf(value));
    }

    /**
     * Adds the not equals constraint.
     */
    @Override
    public void addNotEquals(String binding, Enum<?> value) {
        addNotEquals(binding, String.valueOf(value));
    }

    /**
     * Adds the equals ignore case constraint.
     */
    @Override
    public void addEqualsIgnoreCase(String binding, String value) {
        addEquals(binding, value);
    }

    /**
     * Adds the not equals ignore case constraint.
     */
    @Override
    public void addNotEqualsIgnoreCase(String binding, String value) {
        addNotEquals(binding, value);
    }

    /**
     * Adds the contains constraint.
     */
    @Override
    public void addContains(String binding, String value) {
        WildcardQuery wq = new WildcardQuery(new Term(binding, WILDCARD + lower(escape(value)) + WILDCARD));
        clauses.add(new BooleanClause(wq, MUST));
    }

    /**
     * Adds the not contains constraint.
     */
    @Override
    public void addNotContains(String binding, String value) {
        WildcardQuery wq = new WildcardQuery(new Term(binding, WILDCARD + lower(escape(value)) + WILDCARD));
        clauses.add(new BooleanClause(wq, MUST_NOT));
    }

    /**
     * Adds the starts with constraint.
     */
    @Override
    public void addStartsWith(String binding, String value) {
        WildcardQuery wq = new WildcardQuery(new Term(binding, lower(escape(value)) + WILDCARD));
        clauses.add(new BooleanClause(wq, MUST));
    }

    /**
     * Adds the not starts with constraint.
     */
    @Override
    public void addNotStartsWith(String binding, String value) {
        WildcardQuery wq = new WildcardQuery(new Term(binding, lower(escape(value)) + WILDCARD));
        clauses.add(new BooleanClause(wq, MUST_NOT));
    }

    /**
     * Adds the ends with constraint.
     */
    @Override
    public void addEndsWith(String binding, String value) {
        WildcardQuery wq = new WildcardQuery(new Term(binding, WILDCARD + lower(escape(value))));
        clauses.add(new BooleanClause(wq, MUST));
    }

    /**
     * Prefix wildcards are not supported by lucene
     */
    @Override
    public void addNotEndsWith(String binding, String value) {
        WildcardQuery wq = new WildcardQuery(new Term(binding, WILDCARD + lower(escape(value))));
        clauses.add(new BooleanClause(wq, MUST_NOT));
    }

    /**
     * Adds the greater than constraint.
     */
    @Override
    public void addGreaterThan(String binding, String value) {
        boolean includeLower = false;
        TermRangeQuery query = TermRangeQuery.newStringRange(binding, lower(value), null, includeLower, INCLUDE_UPPER_BOUND);
        clauses.add(new BooleanClause(query, MUST));
    }

    /**
     * Adds the greater than constraint.
     */
    @Override
    public void addGreaterThan(String binding, Date value) {
        String dateStr = formatDate(value);
        addGreaterThan(binding, dateStr);
    }

    /**
     * Adds the greater than constraint.
     */
    @Override
    public void addGreaterThan(String binding, Integer value) {
        int lowerVal = Math.addExact(value, 1);
        Query query = IntField.newRangeQuery(binding, lowerVal, Integer.MAX_VALUE);
        clauses.add(new BooleanClause(query, MUST));
    }

    /**
     * Adds the greater than constraint.
     */
    @Override
    public void addGreaterThan(String binding, Long value) {
        long lowerVal = Math.addExact(value, 1);
        Query query = LongField.newRangeQuery(binding, lowerVal, Long.MAX_VALUE);
        clauses.add(new BooleanClause(query, MUST));
    }

    /**
     * Adds the greater than constraint.
     */
    @Override
    public void addGreaterThan(String binding, Decimal value) {
        Query gt = DoubleField.newRangeQuery(binding, value.doubleValue(), Double.MAX_VALUE);
        clauses.add(new BooleanClause(gt, MUST));

        Query ne = DoubleField.newExactQuery(binding, value.doubleValue());
        clauses.add(new BooleanClause(ne, MUST_NOT));
    }

    /**
     * Adds the greater than or equal to constraint.
     */
    @Override
    public void addGreaterThanOrEqualTo(String binding, String value) {
        TermRangeQuery query = TermRangeQuery.newStringRange(binding, lower(value), null, INCLUDE_LOWER_BOUND,
                INCLUDE_UPPER_BOUND);
        clauses.add(new BooleanClause(query, MUST));
    }

    /**
     * Adds the greater than or equal to constraint.
     */
    @Override
    public void addGreaterThanOrEqualTo(String binding, Date value) {
        addGreaterThanOrEqualTo(binding, formatDate(value));
    }

    /**
     * Adds the greater than or equal to constraint.
     */
    @Override
    public void addGreaterThanOrEqualTo(String binding, Integer value) {
        Query query = IntField.newRangeQuery(binding, value, Integer.MAX_VALUE);
        clauses.add(new BooleanClause(query, MUST));
    }

    /**
     * Adds the greater than or equal to constraint.
     */
    @Override
    public void addGreaterThanOrEqualTo(String binding, Long value) {
        Query query = LongField.newRangeQuery(binding, value, Long.MAX_VALUE);
        clauses.add(new BooleanClause(query, MUST));
    }

    /**
     * Adds the greater than or equal to constraint.
     */
    @Override
    public void addGreaterThanOrEqualTo(String binding, Decimal value) {
        Query gte = DoubleField.newRangeQuery(binding, value.doubleValue(), Double.MAX_VALUE);
        clauses.add(new BooleanClause(gte, MUST));
    }

    /**
     * Adds the less than constraint.
     */
    @Override
    public void addLessThan(String binding, String value) {
        boolean includeUpper = false;
        TermRangeQuery query = TermRangeQuery.newStringRange(binding, null, lower(value), INCLUDE_LOWER_BOUND, includeUpper);
        clauses.add(new BooleanClause(query, MUST));
    }

    /**
     * Adds the less than constraint.
     */
    @Override
    public void addLessThan(String binding, Date value) {
        String dateStr = formatDate(value);
        boolean includeUpper = false;
        TermRangeQuery trq = TermRangeQuery.newStringRange(binding, null, dateStr, INCLUDE_LOWER_BOUND, includeUpper);
        clauses.add(new BooleanClause(trq, MUST));
    }

    /**
     * Adds the less than constraint.
     */
    @Override
    public void addLessThan(String binding, Integer value) {
        int upperVal = Math.addExact(value, -1);
        Query query = IntField.newRangeQuery(binding, Integer.MIN_VALUE, upperVal);
        clauses.add(new BooleanClause(query, MUST));
    }

    /**
     * Adds the less than constraint.
     */
    @Override
    public void addLessThan(String binding, Long value) {
        long upperVal = Math.addExact(value, -1);
        Query query = LongField.newRangeQuery(binding, Long.MIN_VALUE, upperVal);
        clauses.add(new BooleanClause(query, MUST));
    }

    /**
     * Adds the less than constraint.
     */
    @Override
    public void addLessThan(String binding, Decimal value) {
        Query lt = DoubleField.newRangeQuery(binding, Double.MIN_VALUE, value.doubleValue());
        clauses.add(new BooleanClause(lt, MUST));

        Query ne = DoubleField.newExactQuery(binding, value.doubleValue());
        clauses.add(new BooleanClause(ne, MUST_NOT));
    }

    /**
     * Adds the less than or equal to constraint.
     */
    @Override
    public void addLessThanOrEqualTo(String binding, String value) {
        TermRangeQuery query = TermRangeQuery.newStringRange(binding, null, lower(value), INCLUDE_LOWER_BOUND,
                INCLUDE_UPPER_BOUND);
        clauses.add(new BooleanClause(query, MUST));
    }

    /**
     * Adds the less than or equal to constraint.
     */
    @Override
    public void addLessThanOrEqualTo(String binding, Date value) {
        String dateStr = formatDate(value);
        addLessThanOrEqualTo(binding, dateStr);
    }

    /**
     * Adds the less than or equal to constraint.
     */
    @Override
    public void addLessThanOrEqualTo(String binding, Integer value) {
        Query query = IntField.newRangeQuery(binding, Integer.MIN_VALUE, value);
        clauses.add(new BooleanClause(query, MUST));
    }

    /**
     * Adds the less than or equal to constraint.
     */
    @Override
    public void addLessThanOrEqualTo(String binding, Long value) {
        Query query = LongField.newRangeQuery(binding, Long.MIN_VALUE, value);
        clauses.add(new BooleanClause(query, MUST));
    }

    /**
     * Adds the less than or equal to constraint.
     */
    @Override
    public void addLessThanOrEqualTo(String binding, Decimal value) {
        Query lt = DoubleField.newRangeQuery(binding, Double.MIN_VALUE, value.doubleValue());
        clauses.add(new BooleanClause(lt, MUST));
    }

    /**
     * Adds the between constraint.
     */
    @Override
    public void addBetween(String binding, String start, String end) {
        TermRangeQuery rangeQuery = TermRangeQuery.newStringRange(binding, lower(start), lower(end),
                INCLUDE_LOWER_BOUND, INCLUDE_UPPER_BOUND);
        clauses.add(new BooleanClause(rangeQuery, MUST));
    }

    /**
     * Adds the between constraint.
     */
    @Override
    public void addBetween(String binding, Date start, Date end) {
        addBetween(binding, formatDate(start), formatDate(end));
    }

    /**
     * We're reliant here on Skyve having rounded the date to the correct
     * temporal resolution (for equals-like queries anyway).
     * 
     * @param d
     * @return
     */
    private String formatDate(Date d) {
        return DateTools.dateToString(d, Resolution.MILLISECOND);
    }

    /**
     * Adds the between constraint.
     */
    @Override
    public void addBetween(String binding, Integer start, Integer end) {
        Query query = IntField.newRangeQuery(binding, start, end);
        clauses.add(new BooleanClause(query, MUST));
    }

    /**
     * Adds the between constraint.
     */
    @Override
    public void addBetween(String binding, Long start, Long end) {
        Query query = LongField.newRangeQuery(binding, start, end);
        clauses.add(new BooleanClause(query, MUST));
    }

    /**
     * Adds the between constraint.
     */
    @Override
    public void addBetween(String binding, Decimal start, Decimal end) {
        Query between = DoubleField.newRangeQuery(binding, start.doubleValue(), end.doubleValue());
        clauses.add(new BooleanClause(between, MUST));
    }

    /**
     * Adds an inclusion constraint that matches any of the supplied values.
     *
     * @param binding The indexed field name.
     * @param values Candidate values for membership.
     */
    @Override
    public void addIn(String binding, Object... values) {
        List<BytesRef> refs = new ArrayList<>(values.length);

        for (Object val : values) {
            refs.add(new BytesRef(lower(String.valueOf(val))));
        }

        clauses.add(new BooleanClause(new TermInSetQuery(binding, refs), MUST));
    }
    
    /**
     * Adds an exclusion constraint that rejects all supplied values.
     *
     * @param binding The indexed field name.
     * @param values Values that must not match.
     */
    @Override
    public void addNotIn(String binding, Object... values) {
        List<BytesRef> refs = new ArrayList<>(values.length);

        for (Object val : values) {
            refs.add(new BytesRef(lower(String.valueOf(val))));
        }

        clauses.add(new BooleanClause(new TermInSetQuery(binding, refs), MUST_NOT));
    }

    /**
     * Indicates whether no filter clauses have been added.
     *
     * @return {@code true} when this filter has no clauses.
     */
    @Override
    public boolean isEmpty() {
        return clauses.isEmpty();
    }

    /**
     * Returns a debug representation of this filter.
     */
    @Override
    public String toString() {
        return MoreObjects.toStringHelper(this)
                          .add("clauses", clauses)
                          .toString();
    }

    /*
     * Geometry predicates are not supported by archived Lucene list filtering.
     */

    /**
     * Adds the within constraint.
     */
    @Override
    public void addWithin(String binding, Geometry value) {
        throw new UnsupportedOperationException();
    }

    /**
     * Adds the contains constraint.
     */
    @Override
    public void addContains(String binding, Geometry value) {
        throw new UnsupportedOperationException();
    }

    /**
     * Adds the crosses constraint.
     */
    @Override
    public void addCrosses(String binding, Geometry value) {
        throw new UnsupportedOperationException();
    }

    /**
     * Adds the disjoint constraint.
     */
    @Override
    public void addDisjoint(String binding, Geometry value) {
        throw new UnsupportedOperationException();
    }

    /**
     * Adds the intersects constraint.
     */
    @Override
    public void addIntersects(String binding, Geometry value) {
        throw new UnsupportedOperationException();
    }

    /**
     * Adds the overlaps constraint.
     */
    @Override
    public void addOverlaps(String binding, Geometry value) {
        throw new UnsupportedOperationException();
    }

    /**
     * Adds the touches constraint.
     */
    @Override
    public void addTouches(String binding, Geometry value) {
        throw new UnsupportedOperationException();
    }

    /**
     * Adds the not equals constraint.
     */
    @Override
    public void addNotEquals(String binding, Geometry value) {
        throw new UnsupportedOperationException();
    }

    /**
     * Adds the equals constraint.
     */
    @Override
    public void addEquals(String binding, Geometry value) {
        throw new UnsupportedOperationException();
    }
}
