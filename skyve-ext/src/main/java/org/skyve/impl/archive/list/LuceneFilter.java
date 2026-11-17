package org.skyve.impl.archive.list;

import static org.apache.commons.lang3.StringUtils.toRootLowerCase;
import static org.apache.lucene.search.BooleanClause.Occur.FILTER;
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
import org.apache.lucene.search.MatchAllDocsQuery;
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

import jakarta.annotation.Nonnull;

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
    public @Nonnull Query toQuery() {
        Builder builder = new BooleanQuery.Builder();

        boolean positive = false;
        for (BooleanClause clause : clauses) {
            builder.add(clause);
            positive = positive || (clause.getOccur() != MUST_NOT);
        }
        // Lucene matches nothing with only MUST_NOT clauses: exclude them from every document (each document of an
        // archive index is an archived bean), so that a negation ("not equals", "null") matches what it does not exclude
        if ((! clauses.isEmpty()) && (! positive)) {
            builder.add(new MatchAllDocsQuery(), FILTER);
        }

        return builder.build();
    }

    /**
     * Lower case and (lucene) escape the provided string value.
     * 
     * @param value
     * @return
     */
    private static @Nonnull String lower(@Nonnull String value) {
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
    private static @Nonnull String escape(@Nonnull String s) {
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
    public void addAnd(@Nonnull Filter filter) {
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
    public void addOr(@Nonnull Filter filter) {
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
    public void addTagged(@Nonnull String tagId, boolean tagged) {
        throw new UnsupportedOperationException();
    }

    /**
     * Adds the null constraint.
     */
    @Override
    public void addNull(@Nonnull String binding) {
        Query query = new FieldExistsQuery(binding);
        clauses.add(new BooleanClause(query, MUST_NOT));
    }

    /**
     * Adds the not null constraint.
     */
    @Override
    public void addNotNull(@Nonnull String binding) {
        Query query = new FieldExistsQuery(binding);
        clauses.add(new BooleanClause(query, MUST));
    }

    /**
     * Adds the equals constraint.
     */
    @Override
    public void addEquals(@Nonnull String binding, @Nonnull String value) {
        Query query = new TermQuery(new Term(binding, lower(value)));
        clauses.add(new BooleanClause(query, MUST));
    }

    /**
     * Adds the equals constraint.
     */
    @Override
    public void addEquals(@Nonnull String binding, @Nonnull Date value) {
        String dateStr = formatDate(value);
        Query query = new TermQuery(new Term(binding, dateStr));
        clauses.add(new BooleanClause(query, MUST));
    }

    /**
     * Adds the equals constraint.
     */
    @Override
    public void addEquals(@Nonnull String binding, @Nonnull Integer value) {
        Query eq = IntField.newExactQuery(binding, value);
        clauses.add(new BooleanClause(eq, MUST));
    }

    /**
     * Adds the equals constraint.
     */
    @Override
    public void addEquals(@Nonnull String binding, @Nonnull Long value) {
        Query eq = LongField.newExactQuery(binding, value);
        clauses.add(new BooleanClause(eq, MUST));
    }

    /**
     * Adds the equals constraint.
     */
    @Override
    public void addEquals(@Nonnull String binding, @Nonnull Decimal value) {
        Query eq = DoubleField.newExactQuery(binding, value.doubleValue());
        clauses.add(new BooleanClause(eq, MUST));
    }

    /**
     * Adds the equals constraint.
     */
    @Override
    public void addEquals(@Nonnull String binding, @Nonnull Boolean value) {
        addEquals(binding, String.valueOf(value));
    }

    /**
     * Adds the equals constraint.
     */
    @Override
    public void addEquals(@Nonnull String binding, @Nonnull Enum<?> value) {
        addEquals(binding, String.valueOf(value));
    }

    /**
     * Adds the not equals constraint.
     */
    @Override
    public void addNotEquals(@Nonnull String binding, @Nonnull String value) {
        Query query = new TermQuery(new Term(binding, lower(value)));
        clauses.add(new BooleanClause(query, MUST_NOT));
    }

    /**
     * Adds the not equals constraint.
     */
    @Override
    public void addNotEquals(@Nonnull String binding, @Nonnull Date value) {
        String dateStr = formatDate(value);
        addNotEquals(binding, dateStr);
    }

    /**
     * Adds the not equals constraint.
     */
    @Override
    public void addNotEquals(@Nonnull String binding, @Nonnull Integer value) {
        Query iq = IntField.newExactQuery(binding, value);
        clauses.add(new BooleanClause(iq, MUST_NOT));
    }

    /**
     * Adds the not equals constraint.
     */
    @Override
    public void addNotEquals(@Nonnull String binding, @Nonnull Long value) {
        Query iq = LongField.newExactQuery(binding, value);
        clauses.add(new BooleanClause(iq, MUST_NOT));
    }

    /**
     * Adds the not equals constraint.
     */
    @Override
    public void addNotEquals(@Nonnull String binding, @Nonnull Decimal value) {
        Query eq = DoubleField.newExactQuery(binding, value.doubleValue());
        clauses.add(new BooleanClause(eq, MUST_NOT));
    }

    /**
     * Adds the not equals constraint.
     */
    @Override
    public void addNotEquals(@Nonnull String binding, @Nonnull Boolean value) {
        addNotEquals(binding, String.valueOf(value));
    }

    /**
     * Adds the not equals constraint.
     */
    @Override
    public void addNotEquals(@Nonnull String binding, @Nonnull Enum<?> value) {
        addNotEquals(binding, String.valueOf(value));
    }

    /**
     * Adds the equals ignore case constraint.
     */
    @Override
    public void addEqualsIgnoreCase(@Nonnull String binding, @Nonnull String value) {
        addEquals(binding, value);
    }

    /**
     * Adds the not equals ignore case constraint.
     */
    @Override
    public void addNotEqualsIgnoreCase(@Nonnull String binding, @Nonnull String value) {
        addNotEquals(binding, value);
    }

    /**
     * Adds the contains constraint.
     */
    @Override
    public void addContains(@Nonnull String binding, @Nonnull String value) {
        WildcardQuery wq = new WildcardQuery(new Term(binding, WILDCARD + lower(escape(value)) + WILDCARD));
        clauses.add(new BooleanClause(wq, MUST));
    }

    /**
     * Adds the not contains constraint.
     */
    @Override
    public void addNotContains(@Nonnull String binding, @Nonnull String value) {
        WildcardQuery wq = new WildcardQuery(new Term(binding, WILDCARD + lower(escape(value)) + WILDCARD));
        clauses.add(new BooleanClause(wq, MUST_NOT));
    }

    /**
     * Adds the starts with constraint.
     */
    @Override
    public void addStartsWith(@Nonnull String binding, @Nonnull String value) {
        WildcardQuery wq = new WildcardQuery(new Term(binding, lower(escape(value)) + WILDCARD));
        clauses.add(new BooleanClause(wq, MUST));
    }

    /**
     * Adds the not starts with constraint.
     */
    @Override
    public void addNotStartsWith(@Nonnull String binding, @Nonnull String value) {
        WildcardQuery wq = new WildcardQuery(new Term(binding, lower(escape(value)) + WILDCARD));
        clauses.add(new BooleanClause(wq, MUST_NOT));
    }

    /**
     * Adds the ends with constraint.
     */
    @Override
    public void addEndsWith(@Nonnull String binding, @Nonnull String value) {
        WildcardQuery wq = new WildcardQuery(new Term(binding, WILDCARD + lower(escape(value))));
        clauses.add(new BooleanClause(wq, MUST));
    }

    /**
     * Prefix wildcards are not supported by lucene
     */
    @Override
    public void addNotEndsWith(@Nonnull String binding, @Nonnull String value) {
        WildcardQuery wq = new WildcardQuery(new Term(binding, WILDCARD + lower(escape(value))));
        clauses.add(new BooleanClause(wq, MUST_NOT));
    }

    /**
     * Adds the greater than constraint.
     */
    @Override
    public void addGreaterThan(@Nonnull String binding, @Nonnull String value) {
        boolean includeLower = false;
        TermRangeQuery query = TermRangeQuery.newStringRange(binding, lower(value), null, includeLower, INCLUDE_UPPER_BOUND);
        clauses.add(new BooleanClause(query, MUST));
    }

    /**
     * Adds the greater than constraint.
     */
    @Override
    public void addGreaterThan(@Nonnull String binding, @Nonnull Date value) {
        String dateStr = formatDate(value);
        addGreaterThan(binding, dateStr);
    }

    /**
     * Adds the greater than constraint.
     */
    @Override
    public void addGreaterThan(@Nonnull String binding, @Nonnull Integer value) {
        int lowerVal = Math.addExact(value, 1);
        Query query = IntField.newRangeQuery(binding, lowerVal, Integer.MAX_VALUE);
        clauses.add(new BooleanClause(query, MUST));
    }

    /**
     * Adds the greater than constraint.
     */
    @Override
    public void addGreaterThan(@Nonnull String binding, @Nonnull Long value) {
        long lowerVal = Math.addExact(value, 1);
        Query query = LongField.newRangeQuery(binding, lowerVal, Long.MAX_VALUE);
        clauses.add(new BooleanClause(query, MUST));
    }

    /**
     * Adds the greater than constraint.
     */
    @Override
    public void addGreaterThan(@Nonnull String binding, @Nonnull Decimal value) {
        Query gt = DoubleField.newRangeQuery(binding, value.doubleValue(), Double.MAX_VALUE);
        clauses.add(new BooleanClause(gt, MUST));

        Query ne = DoubleField.newExactQuery(binding, value.doubleValue());
        clauses.add(new BooleanClause(ne, MUST_NOT));
    }

    /**
     * Adds the greater than or equal to constraint.
     */
    @Override
    public void addGreaterThanOrEqualTo(@Nonnull String binding, @Nonnull String value) {
        TermRangeQuery query = TermRangeQuery.newStringRange(binding, lower(value), null, INCLUDE_LOWER_BOUND,
                INCLUDE_UPPER_BOUND);
        clauses.add(new BooleanClause(query, MUST));
    }

    /**
     * Adds the greater than or equal to constraint.
     */
    @Override
    public void addGreaterThanOrEqualTo(@Nonnull String binding, @Nonnull Date value) {
        addGreaterThanOrEqualTo(binding, formatDate(value));
    }

    /**
     * Adds the greater than or equal to constraint.
     */
    @Override
    public void addGreaterThanOrEqualTo(@Nonnull String binding, @Nonnull Integer value) {
        Query query = IntField.newRangeQuery(binding, value, Integer.MAX_VALUE);
        clauses.add(new BooleanClause(query, MUST));
    }

    /**
     * Adds the greater than or equal to constraint.
     */
    @Override
    public void addGreaterThanOrEqualTo(@Nonnull String binding, @Nonnull Long value) {
        Query query = LongField.newRangeQuery(binding, value, Long.MAX_VALUE);
        clauses.add(new BooleanClause(query, MUST));
    }

    /**
     * Adds the greater than or equal to constraint.
     */
    @Override
    public void addGreaterThanOrEqualTo(@Nonnull String binding, @Nonnull Decimal value) {
        Query gte = DoubleField.newRangeQuery(binding, value.doubleValue(), Double.MAX_VALUE);
        clauses.add(new BooleanClause(gte, MUST));
    }

    /**
     * Adds the less than constraint.
     */
    @Override
    public void addLessThan(@Nonnull String binding, @Nonnull String value) {
        boolean includeUpper = false;
        TermRangeQuery query = TermRangeQuery.newStringRange(binding, null, lower(value), INCLUDE_LOWER_BOUND, includeUpper);
        clauses.add(new BooleanClause(query, MUST));
    }

    /**
     * Adds the less than constraint.
     */
    @Override
    public void addLessThan(@Nonnull String binding, @Nonnull Date value) {
        String dateStr = formatDate(value);
        boolean includeUpper = false;
        TermRangeQuery trq = TermRangeQuery.newStringRange(binding, null, dateStr, INCLUDE_LOWER_BOUND, includeUpper);
        clauses.add(new BooleanClause(trq, MUST));
    }

    /**
     * Adds the less than constraint.
     */
    @Override
    public void addLessThan(@Nonnull String binding, @Nonnull Integer value) {
        int upperVal = Math.addExact(value, -1);
        Query query = IntField.newRangeQuery(binding, Integer.MIN_VALUE, upperVal);
        clauses.add(new BooleanClause(query, MUST));
    }

    /**
     * Adds the less than constraint.
     */
    @Override
    public void addLessThan(@Nonnull String binding, @Nonnull Long value) {
        long upperVal = Math.addExact(value, -1);
        Query query = LongField.newRangeQuery(binding, Long.MIN_VALUE, upperVal);
        clauses.add(new BooleanClause(query, MUST));
    }

    /**
     * Adds the less than constraint.
     */
    @Override
    public void addLessThan(@Nonnull String binding, @Nonnull Decimal value) {
        Query lt = DoubleField.newRangeQuery(binding, Double.MIN_VALUE, value.doubleValue());
        clauses.add(new BooleanClause(lt, MUST));

        Query ne = DoubleField.newExactQuery(binding, value.doubleValue());
        clauses.add(new BooleanClause(ne, MUST_NOT));
    }

    /**
     * Adds the less than or equal to constraint.
     */
    @Override
    public void addLessThanOrEqualTo(@Nonnull String binding, @Nonnull String value) {
        TermRangeQuery query = TermRangeQuery.newStringRange(binding, null, lower(value), INCLUDE_LOWER_BOUND,
                INCLUDE_UPPER_BOUND);
        clauses.add(new BooleanClause(query, MUST));
    }

    /**
     * Adds the less than or equal to constraint.
     */
    @Override
    public void addLessThanOrEqualTo(@Nonnull String binding, @Nonnull Date value) {
        String dateStr = formatDate(value);
        addLessThanOrEqualTo(binding, dateStr);
    }

    /**
     * Adds the less than or equal to constraint.
     */
    @Override
    public void addLessThanOrEqualTo(@Nonnull String binding, @Nonnull Integer value) {
        Query query = IntField.newRangeQuery(binding, Integer.MIN_VALUE, value);
        clauses.add(new BooleanClause(query, MUST));
    }

    /**
     * Adds the less than or equal to constraint.
     */
    @Override
    public void addLessThanOrEqualTo(@Nonnull String binding, @Nonnull Long value) {
        Query query = LongField.newRangeQuery(binding, Long.MIN_VALUE, value);
        clauses.add(new BooleanClause(query, MUST));
    }

    /**
     * Adds the less than or equal to constraint.
     */
    @Override
    public void addLessThanOrEqualTo(@Nonnull String binding, @Nonnull Decimal value) {
        Query lt = DoubleField.newRangeQuery(binding, Double.MIN_VALUE, value.doubleValue());
        clauses.add(new BooleanClause(lt, MUST));
    }

    /**
     * Adds the between constraint.
     */
    @Override
    public void addBetween(@Nonnull String binding, @Nonnull String start, @Nonnull String end) {
        TermRangeQuery rangeQuery = TermRangeQuery.newStringRange(binding, lower(start), lower(end),
                INCLUDE_LOWER_BOUND, INCLUDE_UPPER_BOUND);
        clauses.add(new BooleanClause(rangeQuery, MUST));
    }

    /**
     * Adds the between constraint.
     */
    @Override
    public void addBetween(@Nonnull String binding, @Nonnull Date start, @Nonnull Date end) {
        addBetween(binding, formatDate(start), formatDate(end));
    }

    /**
     * We're reliant here on Skyve having rounded the date to the correct
     * temporal resolution (for equals-like queries anyway).
     * 
     * @param d
     * @return
     */
    private @Nonnull String formatDate(@Nonnull Date d) {
        return DateTools.dateToString(d, Resolution.MILLISECOND);
    }

    /**
     * Adds the between constraint.
     */
    @Override
    public void addBetween(@Nonnull String binding, @Nonnull Integer start, @Nonnull Integer end) {
        Query query = IntField.newRangeQuery(binding, start, end);
        clauses.add(new BooleanClause(query, MUST));
    }

    /**
     * Adds the between constraint.
     */
    @Override
    public void addBetween(@Nonnull String binding, @Nonnull Long start, @Nonnull Long end) {
        Query query = LongField.newRangeQuery(binding, start, end);
        clauses.add(new BooleanClause(query, MUST));
    }

    /**
     * Adds the between constraint.
     */
    @Override
    public void addBetween(@Nonnull String binding, @Nonnull Decimal start, @Nonnull Decimal end) {
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
    public void addIn(@Nonnull String binding, @Nonnull Object... values) {
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
    public void addNotIn(@Nonnull String binding, @Nonnull Object... values) {
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
    public @Nonnull String toString() {
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
    public void addWithin(@Nonnull String binding, @Nonnull Geometry value) {
        throw new UnsupportedOperationException();
    }

    /**
     * Adds the contains constraint.
     */
    @Override
    public void addContains(@Nonnull String binding, @Nonnull Geometry value) {
        throw new UnsupportedOperationException();
    }

    /**
     * Adds the crosses constraint.
     */
    @Override
    public void addCrosses(@Nonnull String binding, @Nonnull Geometry value) {
        throw new UnsupportedOperationException();
    }

    /**
     * Adds the disjoint constraint.
     */
    @Override
    public void addDisjoint(@Nonnull String binding, @Nonnull Geometry value) {
        throw new UnsupportedOperationException();
    }

    /**
     * Adds the intersects constraint.
     */
    @Override
    public void addIntersects(@Nonnull String binding, @Nonnull Geometry value) {
        throw new UnsupportedOperationException();
    }

    /**
     * Adds the overlaps constraint.
     */
    @Override
    public void addOverlaps(@Nonnull String binding, @Nonnull Geometry value) {
        throw new UnsupportedOperationException();
    }

    /**
     * Adds the touches constraint.
     */
    @Override
    public void addTouches(@Nonnull String binding, @Nonnull Geometry value) {
        throw new UnsupportedOperationException();
    }

    /**
     * Adds the not equals constraint.
     */
    @Override
    public void addNotEquals(@Nonnull String binding, @Nonnull Geometry value) {
        throw new UnsupportedOperationException();
    }

    /**
     * Adds the equals constraint.
     */
    @Override
    public void addEquals(@Nonnull String binding, @Nonnull Geometry value) {
        throw new UnsupportedOperationException();
    }
}
