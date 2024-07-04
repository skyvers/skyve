package modules.admin.Audit.models;

import static org.apache.commons.lang3.StringUtils.toRootLowerCase;
import static org.apache.lucene.queryparser.classic.QueryParserBase.escape;
import static org.apache.lucene.search.BooleanClause.Occur.MUST;
import static org.apache.lucene.search.BooleanClause.Occur.MUST_NOT;
import static org.apache.lucene.search.BooleanClause.Occur.SHOULD;

import java.io.IOException;
import java.nio.file.Path;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.apache.lucene.document.DateTools;
import org.apache.lucene.document.DateTools.Resolution;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.IntField;
import org.apache.lucene.document.LongField;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.IndexableField;
import org.apache.lucene.index.Term;
import org.apache.lucene.search.BooleanClause;
import org.apache.lucene.search.BooleanQuery;
import org.apache.lucene.search.BooleanQuery.Builder;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.Sort;
import org.apache.lucene.search.SortField;
import org.apache.lucene.search.SortField.Type;
import org.apache.lucene.search.TermInSetQuery;
import org.apache.lucene.search.TermQuery;
import org.apache.lucene.search.TermRangeQuery;
import org.apache.lucene.search.TopDocs;
import org.apache.lucene.search.WildcardQuery;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;
import org.apache.lucene.util.BytesRef;
import org.locationtech.jts.geom.Geometry;
import org.skyve.domain.types.Decimal;
import org.skyve.metadata.view.model.list.Filter;

import com.google.common.base.MoreObjects;
import com.google.common.base.Stopwatch;

public class LuceneFilter implements Filter {

    private static final String WILDCARD = "*";
    private static final boolean INCLUDE_LOWER_BOUND = true;
    private static final boolean INCLUDE_UPPER_BOUND = true;

    private List<BooleanClause> clauses = new ArrayList<>();

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
    private static String lowerEscape(String value) {
        return toRootLowerCase(escape(value));
    }

    @Override
    public void addAnd(Filter filter) {

        if (filter instanceof LuceneFilter lf) {
            clauses.add(new BooleanClause(lf.toQuery(), MUST));
        }
    }

    @Override
    public void addOr(Filter filter) {

        if (filter instanceof LuceneFilter lf) {
            clauses.add(new BooleanClause(lf.toQuery(), SHOULD));
        }
    }

    @Override
    public void addTagged(String tagId, boolean tagged) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void addNull(String binding) {
        // FIXME doesn't work
        TermRangeQuery trq = TermRangeQuery.newStringRange(binding, null, null, true, true);
        clauses.add(new BooleanClause(trq, MUST_NOT));
    }

    @Override
    public void addNotNull(String binding) {
        // FIXME doesn't work
        TermRangeQuery trq = TermRangeQuery.newStringRange(binding, null, null, true, true);
        clauses.add(new BooleanClause(trq, MUST));
    }

    @Override
    public void addEquals(String binding, String value) {

        Query query = new TermQuery(new Term(binding, lowerEscape(value)));
        clauses.add(new BooleanClause(query, MUST));
    }

    @Override
    public void addEquals(String binding, Date value) {

        String dateStr = formatDate(value);
        Query query = new TermQuery(new Term(binding, dateStr));
        clauses.add(new BooleanClause(query, MUST));
    }

    @Override
    public void addEquals(String binding, Integer value) {

        Query eq = IntField.newExactQuery(binding, value);
        clauses.add(new BooleanClause(eq, MUST));
    }

    @Override
    public void addEquals(String binding, Long value) {
        Query eq = LongField.newExactQuery(binding, value);
        clauses.add(new BooleanClause(eq, MUST));
    }

    @Override
    public void addEquals(String binding, Decimal value) {
        // TODO handle Decimal
        throw new UnsupportedOperationException();
    }

    @Override
    public void addEquals(String binding, Boolean value) {

        addEquals(binding, String.valueOf(value));
    }

    @Override
    public void addEquals(String binding, Enum<?> value) {

        addEquals(binding, String.valueOf(value));
    }

    @Override
    public void addNotEquals(String binding, String value) {
        Query query = new TermQuery(new Term(binding, lowerEscape(value)));
        clauses.add(new BooleanClause(query, MUST_NOT));
    }

    @Override
    public void addNotEquals(String binding, Date value) {

        String dateStr = formatDate(value);
        addNotEquals(binding, dateStr);
    }

    @Override
    public void addNotEquals(String binding, Integer value) {
        Query iq = IntField.newExactQuery(binding, value);
        clauses.add(new BooleanClause(iq, MUST_NOT));
    }

    @Override
    public void addNotEquals(String binding, Long value) {
        Query iq = LongField.newExactQuery(binding, value);
        clauses.add(new BooleanClause(iq, MUST_NOT));
    }

    @Override
    public void addNotEquals(String binding, Decimal value) {
        // TODO handle Decimal
        throw new UnsupportedOperationException();
    }

    @Override
    public void addNotEquals(String binding, Boolean value) {
        addNotEquals(binding, String.valueOf(value));
    }

    @Override
    public void addNotEquals(String binding, Enum<?> value) {
        addNotEquals(binding, String.valueOf(value));
    }

    @Override
    public void addEqualsIgnoreCase(String binding, String value) {
        addEquals(binding, value);
    }

    @Override
    public void addNotEqualsIgnoreCase(String binding, String value) {
        addNotEquals(binding, value);
    }

    @Override
    public void addContains(String binding, String value) {
        WildcardQuery wq = new WildcardQuery(new Term(binding, WILDCARD + lowerEscape(value) + WILDCARD));
        clauses.add(new BooleanClause(wq, MUST));
    }

    @Override
    public void addNotContains(String binding, String value) {
        WildcardQuery wq = new WildcardQuery(new Term(binding, WILDCARD + lowerEscape(value) + WILDCARD));
        clauses.add(new BooleanClause(wq, MUST_NOT));
    }

    @Override
    public void addStartsWith(String binding, String value) {

        WildcardQuery wq = new WildcardQuery(new Term(binding, lowerEscape(value) + WILDCARD));
        clauses.add(new BooleanClause(wq, MUST));
    }

    @Override
    public void addNotStartsWith(String binding, String value) {
        WildcardQuery wq = new WildcardQuery(new Term(binding, lowerEscape(value) + WILDCARD));
        clauses.add(new BooleanClause(wq, MUST_NOT));
    }

    @Override
    public void addEndsWith(String binding, String value) {
        WildcardQuery wq = new WildcardQuery(new Term(binding, WILDCARD + lowerEscape(value)));
        clauses.add(new BooleanClause(wq, MUST));
    }

    /**
     * Prefix wildcards are not supported by lucene
     */
    @Override
    public void addNotEndsWith(String binding, String value) {
        WildcardQuery wq = new WildcardQuery(new Term(binding, WILDCARD + lowerEscape(value)));
        clauses.add(new BooleanClause(wq, MUST_NOT));
    }

    @Override
    public void addGreaterThan(String binding, String value) {

        boolean includeLower = false;
        TermRangeQuery query = TermRangeQuery.newStringRange(binding, lowerEscape(value), null, includeLower, INCLUDE_UPPER_BOUND);
        clauses.add(new BooleanClause(query, MUST));
    }

    @Override
    public void addGreaterThan(String binding, Date value) {
        String dateStr = formatDate(value);
        addGreaterThan(binding, dateStr);
    }

    @Override
    public void addGreaterThan(String binding, Integer value) {
        int lowerVal = Math.addExact(value, 1);
        Query query = IntField.newRangeQuery(binding, lowerVal, Integer.MAX_VALUE);
        clauses.add(new BooleanClause(query, MUST));
    }

    @Override
    public void addGreaterThan(String binding, Long value) {
        long lowerVal = Math.addExact(value, 1);
        Query query = LongField.newRangeQuery(binding, lowerVal, Long.MAX_VALUE);
        clauses.add(new BooleanClause(query, MUST));
    }

    @Override
    public void addGreaterThan(String binding, Decimal value) {
        // TODO handle Decimal
        throw new UnsupportedOperationException();
    }

    @Override
    public void addGreaterThanOrEqualTo(String binding, String value) {
        TermRangeQuery query = TermRangeQuery.newStringRange(binding, lowerEscape(value), null, INCLUDE_LOWER_BOUND,
                INCLUDE_UPPER_BOUND);
        clauses.add(new BooleanClause(query, MUST));
    }

    @Override
    public void addGreaterThanOrEqualTo(String binding, Date value) {
        addGreaterThanOrEqualTo(binding, formatDate(value));
    }

    @Override
    public void addGreaterThanOrEqualTo(String binding, Integer value) {
        Query query = IntField.newRangeQuery(binding, value, Integer.MAX_VALUE);
        clauses.add(new BooleanClause(query, MUST));
    }

    @Override
    public void addGreaterThanOrEqualTo(String binding, Long value) {
        Query query = LongField.newRangeQuery(binding, value, Long.MAX_VALUE);
        clauses.add(new BooleanClause(query, MUST));
    }

    @Override
    public void addGreaterThanOrEqualTo(String binding, Decimal value) {
        // TODO handle Decimal
        throw new UnsupportedOperationException();
    }

    @Override
    public void addLessThan(String binding, String value) {

        boolean includeUpper = false;
        TermRangeQuery query = TermRangeQuery.newStringRange(binding, null, lowerEscape(value), INCLUDE_LOWER_BOUND, includeUpper);
        clauses.add(new BooleanClause(query, MUST));
    }

    @Override
    public void addLessThan(String binding, Date value) {

        String dateStr = formatDate(value);
        boolean includeUpper = false;
        TermRangeQuery trq = TermRangeQuery.newStringRange(binding, null, dateStr, INCLUDE_LOWER_BOUND, includeUpper);
        clauses.add(new BooleanClause(trq, MUST));
    }

    @Override
    public void addLessThan(String binding, Integer value) {

        int upperVal = Math.addExact(value, -1);
        Query query = IntField.newRangeQuery(binding, Integer.MIN_VALUE, upperVal);
        clauses.add(new BooleanClause(query, MUST));
    }

    @Override
    public void addLessThan(String binding, Long value) {

        long upperVal = Math.addExact(value, -1);
        Query query = LongField.newRangeQuery(binding, Long.MIN_VALUE, upperVal);
        clauses.add(new BooleanClause(query, MUST));
    }

    @Override
    public void addLessThan(String binding, Decimal value) {
        // TODO handle Decimal
        throw new UnsupportedOperationException();
    }

    @Override
    public void addLessThanOrEqualTo(String binding, String value) {
        TermRangeQuery query = TermRangeQuery.newStringRange(binding, null, lowerEscape(value), INCLUDE_LOWER_BOUND,
                INCLUDE_UPPER_BOUND);
        clauses.add(new BooleanClause(query, MUST));
    }

    @Override
    public void addLessThanOrEqualTo(String binding, Date value) {
        String dateStr = formatDate(value);
        addLessThanOrEqualTo(binding, dateStr);
    }

    @Override
    public void addLessThanOrEqualTo(String binding, Integer value) {
        Query query = IntField.newRangeQuery(binding, Integer.MIN_VALUE, value);
        clauses.add(new BooleanClause(query, MUST));
    }

    @Override
    public void addLessThanOrEqualTo(String binding, Long value) {
        Query query = LongField.newRangeQuery(binding, Long.MIN_VALUE, value);
        clauses.add(new BooleanClause(query, MUST));
    }

    @Override
    public void addLessThanOrEqualTo(String binding, Decimal value) {
        // TODO handle Decimals
        throw new UnsupportedOperationException();
    }

    @Override
    public void addBetween(String binding, String start, String end) {

        TermRangeQuery rangeQuery = TermRangeQuery.newStringRange(binding, lowerEscape(start), lowerEscape(end),
                INCLUDE_LOWER_BOUND, INCLUDE_UPPER_BOUND);
        clauses.add(new BooleanClause(rangeQuery, MUST));
    }

    @Override
    public void addBetween(String binding, Date start, Date end) {

        // FIXME need to jiggle dates properly
        addBetween(binding, formatDate(start), formatDate(end));
    }

    /**
     * TODO we may need some rounding functions for the range stuff, unless skyve handles it.
     * 
     * @param d
     * @return
     */
    private String formatDate(Date d) {
        return DateTools.dateToString(d, Resolution.MILLISECOND);
    }

    @Override
    public void addBetween(String binding, Integer start, Integer end) {
        Query query = IntField.newRangeQuery(binding, start, end);
        clauses.add(new BooleanClause(query, MUST));
    }

    @Override
    public void addBetween(String binding, Long start, Long end) {

        Query query = LongField.newRangeQuery(binding, start, end);
        clauses.add(new BooleanClause(query, MUST));
    }

    @Override
    public void addBetween(String binding, Decimal start, Decimal end) {
        // TODO handle Decimals
        throw new UnsupportedOperationException();
    }

    @Override
    public void addIn(String binding, Object... values) {

        List<BytesRef> refs = new ArrayList<>(values.length);

        for (Object val : values) {
            refs.add(new BytesRef(String.valueOf(val)));
        }

        clauses.add(new BooleanClause(new TermInSetQuery(binding, refs), MUST));
    }

    @Override
    public boolean isEmpty() {
        return clauses.isEmpty();
    }

    @Override
    public String toString() {
        return MoreObjects.toStringHelper(this)
                          .add("clauses", clauses)
                          .toString();
    }

    /**
     * Not supporting any Geometry queries
     */

    @Override
    public void addWithin(String binding, Geometry value) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void addContains(String binding, Geometry value) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void addCrosses(String binding, Geometry value) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void addDisjoint(String binding, Geometry value) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void addIntersects(String binding, Geometry value) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void addOverlaps(String binding, Geometry value) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void addTouches(String binding, Geometry value) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void addNotEquals(String binding, Geometry value) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void addEquals(String binding, Geometry value) {
        throw new UnsupportedOperationException();
    }

    public static class QueryTemp {

        public static void go(Query query) throws IOException, ParseException {

            Stopwatch t = Stopwatch.createStarted();

            // try (Directory directory = FSDirectory.open(Path.of("C:\\data\\lucene-test-data\\audit-index-01"));
            try (Directory directory = FSDirectory.open(Path.of("C:\\data\\skyve-content\\skyve\\archive\\index"));
                    DirectoryReader ireader = DirectoryReader.open(directory)) {

                IndexSearcher isearcher = new IndexSearcher(ireader);

                queryAndLog(ireader, isearcher, query);

                System.out.println("\ndone, took " + t);
            }

        }

        private static void queryAndLog(DirectoryReader ireader, IndexSearcher isearcher, Query query)
                throws IOException, ParseException {

            System.out.println();
            System.out.println("Using Query: " + query);
            System.out.println();

            TopDocs td = isearcher.search(query, 20);

            String howManyMsg = td.totalHits + " [" + StringUtils.repeat('#', (int) td.totalHits.value) + "]";
            System.out.println(howManyMsg);

            for (ScoreDoc score : td.scoreDocs) {

                Document doc = ireader.storedFields()
                                      .document(score.doc);

                System.out.println();

                hr("Doc num: " + score.doc + "; score: " + score.score);
                printSomeDetails(doc);
                hr("");
            }

            System.out.println();
            System.out.println(howManyMsg);
        }

        private static void printSomeDetails(Document doc) throws ParseException {

            for (IndexableField field : doc) {

                String name = StringUtils.leftPad(field.name(), 18);
                String val = field.stringValue();

                if ("timestamp".equals(field.name())) {
                    val = DateTools.stringToDate(val) + " [" + val + "]";
                }

                System.out.printf("# %s: %s %n", name, val);

            }

        }

        private static void hr(String label) {
            if (label.isBlank()) {
                System.out.println(StringUtils.repeat('=', 70));
            } else {
                String s = "== " + StringUtils.rightPad(label + " ", 67, '=');
                System.out.println(s);
            }
        }
    }

    public static void main(String[] args) throws Exception {

        System.out.println(StringUtils.repeat('=', 72));
        LuceneFilter test = new LuceneFilter();

        // test.addContains("auditModuleName", "admin");
        // test.addIn("auditDocumentName", "snapshot");
        test.addIn("operation", "insert");

        System.out.println(test);
        System.out.println(test.toQuery());

        QueryTemp.go(test.toQuery());
    }
}
