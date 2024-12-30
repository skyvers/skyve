package org.skyve.impl.archive.list;

import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toSet;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.lessThan;
import static org.hamcrest.Matchers.notNullValue;

import java.io.IOException;
import java.time.Duration;
import java.time.Instant;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Stream;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.core.KeywordTokenizerFactory;
import org.apache.lucene.analysis.core.LowerCaseFilterFactory;
import org.apache.lucene.analysis.custom.CustomAnalyzer;
import org.apache.lucene.document.DateTools;
import org.apache.lucene.document.DateTools.Resolution;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.DoubleField;
import org.apache.lucene.document.Field.Store;
import org.apache.lucene.document.IntField;
import org.apache.lucene.document.LongField;
import org.apache.lucene.document.TextField;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.index.StoredFields;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.TopDocs;
import org.apache.lucene.store.ByteBuffersDirectory;
import org.apache.lucene.store.Directory;
import org.junit.Test;
import org.skyve.domain.types.Decimal;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;

import com.google.common.util.concurrent.AtomicDouble;

/**
 * NB because of how lucene works we often need to add some criteria to give
 * a positive score to documents otherwise we will get zero results; typically
 * I've used addNotNull().
 */
@SuppressWarnings({"boxing", "static-method"})
public class LuceneFilterTest {

    private static final String DECIMAL_FIELD = "our_decimal";
    private static final String DOUBLE_FIELD = "our_doub";
    private static final String LONG_FIELD = "our_long";
    private static final String DATE_FIELD = "our_date";
    private static final String TEXT_FIELD = "some_text";
    private static final String INT_FIELD = "our_int";
    private static final String BOOLEAN_FIELD = "our_bool";
    private static final String ENUM_FIELD = "our_enum";

    private final int MAX_RESULTS = 25;

    private static final boolean debugPrint = false;

    private enum TestEnum {
        X, Y, Z
    }

    @Test
    public void testContainsBasic() {

        String testTextVal = "this is a test text field";
        List<Document> docs = List.of(createDoc(testTextVal, 1.1), createDoc("FOOBAR", 1.1));

        LuceneFilter filter = new LuceneFilter();
        filter.addContains(TEXT_FIELD, "test");

        List<Document> results = newIndexAndQuery(docs, filter);

        assertThat(results.size(), is(1));
        Document firstResult = results.get(0);
        assertThat(firstResult.get(TEXT_FIELD), is(testTextVal));

        assertThat(filter.toString(), notNullValue());
        assertThat(filter.isEmpty(), is(false));
    }

    @Test
    public void testContainsOr() {

        List<Document> docs = List.of(
                createDoc("test baz", 1.1),
                createDoc("test bar", 1.1),
                createDoc("test foo", 1.1),
                createDoc("quick", 333),
                createDoc("brown", 666),
                createDoc("fox", 999));

        LuceneFilter filterA = new LuceneFilter();
        filterA.addContains(TEXT_FIELD, "foo");

        LuceneFilter filterB = new LuceneFilter();
        filterB.addContains(TEXT_FIELD, "bar");

        LuceneFilter filterC = new LuceneFilter();
        filterC.addContains(TEXT_FIELD, "baz");

        filterA.addOr(filterB);
        filterA.addOr(filterC);

        List<Document> results = newIndexAndQuery(docs, filterA);

        // Expecting 3 results
        assertThat(results.size(), is(3));

        // Dobule value for the correct results are all sub 10
        for (Document document : results) {

            double d = document.getField(DOUBLE_FIELD)
                               .numericValue()
                               .doubleValue();
            assertThat(d, is(lessThan(10.0)));
        }
    }

    @Test
    public void testContainsAnd() {

        List<Document> docs = List.of(
                createDoc("test baz", 777),
                createDoc("test bar", 1.1),
                createDoc("test foo", 888),
                createDoc("quick", 333),
                createDoc("brown", 666),
                createDoc("fox", 999));

        LuceneFilter filterA = new LuceneFilter();
        filterA.addContains(TEXT_FIELD, "test");

        LuceneFilter filterB = new LuceneFilter();
        filterB.addContains(TEXT_FIELD, "bar");

        filterA.addAnd(filterB);

        List<Document> results = newIndexAndQuery(docs, filterA);

        // Expecting 3 results
        assertThat(results.size(), is(1));

        // Dobule value for the correct results are all sub 10
        for (Document document : results) {

            double d = document.getField(DOUBLE_FIELD)
                               .numericValue()
                               .doubleValue();
            assertThat(d, is(lessThan(10.0)));
        }
    }

    @Test
    public void testNull() {

        Document target = new Document();
        target.add(new DoubleField(DOUBLE_FIELD, 4.4, Store.YES));

        List<Document> docs = List.of(createDoc("BAZ", 999), target, createDoc("FOOBAR", 888));

        LuceneFilter filter = new LuceneFilter();
        // We can't have *only* a negative query, which (confusingly) addNull is
        filter.addNull(TEXT_FIELD);

        // so we need to include a positive part to the query
        filter.addNotNull(DOUBLE_FIELD);

        List<Document> results = newIndexAndQuery(docs, filter);

        assertThat(results.size(), is(1));
        assertThat(results.get(0)
                          .getField(DOUBLE_FIELD)
                          .numericValue()
                          .doubleValue(),
                is(lessThan(10.0)));
    }

    @Test
    public void testNotNull() {

        Document target = new Document();
        target.add(new DoubleField(DOUBLE_FIELD, 4.4, Store.YES));

        List<Document> docs = List.of(createDoc("some text", 1.5), target, createDoc("FOOBAR", 2.5));

        LuceneFilter filter = new LuceneFilter();
        filter.addNotNull(DOUBLE_FIELD);

        List<Document> results = newIndexAndQuery(docs, filter);

        assertThat(results.size(), is(3));

        for (Document document : results) {

            assertThat(document.getField(DOUBLE_FIELD)
                               .numericValue()
                               .doubleValue(),
                    is(lessThan(10.0)));
        }
    }

    @Test
    public void testEqualsString() {

        // Make sure we get 0 results first
        {
            List<Document> docs = List.of(createDoc("FOOBAR", 999), createDoc("BAZ", 999));

            LuceneFilter filter = new LuceneFilter();
            filter.addEquals(TEXT_FIELD, "FOO");

            List<Document> results = newIndexAndQuery(docs, filter);

            assertThat(results.size(), is(0));
        }

        // Then get 1 hit
        {
            List<Document> docs = List.of(createDoc("FOOBAR", 1.1), createDoc("BAZ", 999));

            LuceneFilter filter = new LuceneFilter();
            filter.addEqualsIgnoreCase(TEXT_FIELD, "foobar");

            List<Document> results = newIndexAndQuery(docs, filter);

            assertThat(results.size(), is(1));
            assertThat(results.get(0)
                              .getField(DOUBLE_FIELD)
                              .numericValue()
                              .doubleValue(),
                    is(lessThan(10.0)));
        }
    }

    @Test
    public void testNotEqualsString() {

        List<Document> docs = List.of(
                createDoc("FOOBAR", 1.1),
                createDoc("BAZ", 999),
                createDoc("xBAZ", 4.4));

        LuceneFilter filter = new LuceneFilter();
        filter.addNotEqualsIgnoreCase(TEXT_FIELD, "BAZ");
        filter.addNotNull(TEXT_FIELD);
        // Not null required to given some positive score to all docs

        List<Document> results = newIndexAndQuery(docs, filter);

        assertThat(results.size(), is(2));
        for (Document document : results) {
            assertThat(getDouble(document), is(lessThan(10.0)));
        }
    }

    @Test
    public void testEqualsDate() {

        Date now = new Date();
        Date future = Date.from(Instant.now()
                                       .plus(Duration.ofHours(1)));

        List<Document> docs = List.of(createDoc(now, 1.1), createDoc(future, 999));

        LuceneFilter filter = new LuceneFilter();
        filter.addEquals(DATE_FIELD, now);

        List<Document> results = newIndexAndQuery(docs, filter);

        assertThat(results.size(), is(1));
        assertThat(getDouble(results.get(0)), is(lessThan(10.0)));
    }

    @Test
    public void testNotEqualsDate() {

        Date now = new Date();
        Date future = Date.from(Instant.now()
                                       .plus(Duration.ofHours(1)));

        List<Document> docs = List.of(createDoc(now, 777), createDoc(future, 4));

        LuceneFilter filter = new LuceneFilter();
        filter.addNotNull(DATE_FIELD);
        filter.addNotEquals(DATE_FIELD, now);

        List<Document> results = newIndexAndQuery(docs, filter);

        assertThat(results.size(), is(1));
        assertThat(getDouble(results.get(0)), is(lessThan(10.0)));
    }

    @Test
    public void testEqualsInteger() {

        int targetValue = 4;
        List<Document> docs = List.of(createDoc(777), createDoc(targetValue), createDoc(555));

        LuceneFilter filter = new LuceneFilter();
        filter.addEquals(INT_FIELD, targetValue);

        List<Document> results = newIndexAndQuery(docs, filter);

        assertThat(results.size(), is(1));
        assertThat(getInt(results.get(0)), is(targetValue));
    }

    @Test
    public void testNotEqualsInteger() {

        int targetValue = 444;
        List<Document> docs = List.of(createDoc(2), createDoc(targetValue), createDoc(3));

        LuceneFilter filter = new LuceneFilter();
        filter.addNotNull(INT_FIELD);
        filter.addNotEquals(INT_FIELD, targetValue);

        List<Document> results = newIndexAndQuery(docs, filter);

        assertThat(results.size(), is(2));
        for (Document document : results) {

            assertThat(getInt(document), not(targetValue));
        }
    }

    @Test
    public void testEqualsLong() {

        long targetValue = Long.MAX_VALUE;
        List<Document> docs = List.of(createDoc(Long.MIN_VALUE), createDoc(targetValue), createDoc(555l));

        LuceneFilter filter = new LuceneFilter();
        filter.addEquals(LONG_FIELD, targetValue);

        List<Document> results = newIndexAndQuery(docs, filter);

        assertThat(results.size(), is(1));
        assertThat(getLong(results.get(0)), is(targetValue));
    }

    @Test
    public void testNotEqualsLong() {

        long targetValue = Long.MAX_VALUE;
        List<Document> docs = List.of(createDoc(Long.MIN_VALUE), createDoc(targetValue), createDoc(555l));

        LuceneFilter filter = new LuceneFilter();
        filter.addNotNull(LONG_FIELD);
        filter.addNotEquals(LONG_FIELD, targetValue);

        List<Document> results = newIndexAndQuery(docs, filter);

        assertThat(results.size(), is(2));
        for (Document document : results) {
            assertThat(getLong(document), not(targetValue));
        }
    }

    @Test
    public void testEqualsDecimal() {

        Decimal targetValue = new Decimal5("5.45");

        List<Document> docs = List.of(
                createDoc(Decimal2.ZERO),
                createDoc(new Decimal5("5.451")),
                createDoc(targetValue),
                createDoc(Decimal2.ONE_THOUSAND));

        LuceneFilter filter = new LuceneFilter();
        filter.addEquals(DECIMAL_FIELD, targetValue);

        List<Document> results = newIndexAndQuery(docs, filter);

        assertThat(results.size(), is(1));
        for (Document document : results) {
            assertThat(document.getField(DECIMAL_FIELD)
                               .numericValue()
                               .doubleValue(),
                    is(targetValue.doubleValue()));
        }
    }

    @Test
    public void testNotEqualsDecimal() {

        Decimal targetValue = new Decimal5("5.45");

        List<Document> docs = List.of(
                createDoc(Decimal2.ZERO),
                createDoc(new Decimal5("5.451")),
                createDoc(targetValue),
                createDoc(Decimal2.ONE_THOUSAND));

        LuceneFilter filter = new LuceneFilter();
        filter.addNotNull(DECIMAL_FIELD);
        filter.addNotEquals(DECIMAL_FIELD, targetValue);

        List<Document> results = newIndexAndQuery(docs, filter);

        assertThat(results.size(), is(3));

        for (Document document : results) {
            assertThat(document.getField(DECIMAL_FIELD)
                               .numericValue()
                               .doubleValue(),
                    not(targetValue.doubleValue()));
        }
    }

    @Test
    public void testEqualsBoolean() {

        List<Document> docs = List.of(createDoc(true), createDoc(false), createDoc(true));

        LuceneFilter filter = new LuceneFilter();
        filter.addEquals(BOOLEAN_FIELD, true);

        List<Document> results = newIndexAndQuery(docs, filter);

        assertThat(results.size(), is(2));
        for (Document document : results) {
            assertThat(document.get(BOOLEAN_FIELD), is("true"));
        }

    }

    @Test
    public void testNotEqualsBoolean() {

        List<Document> docs = List.of(createDoc(true), createDoc(false), createDoc(true));

        LuceneFilter filter = new LuceneFilter();
        filter.addNotNull(BOOLEAN_FIELD);
        filter.addNotEquals(BOOLEAN_FIELD, true);

        List<Document> results = newIndexAndQuery(docs, filter);

        assertThat(results.size(), is(1));
        for (Document document : results) {
            assertThat(document.get(BOOLEAN_FIELD), is("false"));
        }
    }

    @Test
    public void testEqualsEnum() {

        List<Document> docs = List.of(
                createDoc(TestEnum.Y),
                createDoc(TestEnum.X),
                createDoc(TestEnum.Y),
                createDoc(TestEnum.Z));

        LuceneFilter filter = new LuceneFilter();
        filter.addEquals(ENUM_FIELD, TestEnum.Y);

        List<Document> results = newIndexAndQuery(docs, filter);

        assertThat(results.size(), is(2));
        for (Document document : results) {
            assertThat(document.get(ENUM_FIELD), is("Y"));
        }
    }

    @Test
    public void testEqualsIn() {

        List<Document> docs = List.of(
                createDoc(TestEnum.Y),
                createDoc(TestEnum.X),
                createDoc(TestEnum.Y),
                createDoc(TestEnum.Z));

        LuceneFilter filter = new LuceneFilter();
        filter.addIn(ENUM_FIELD, TestEnum.Y, TestEnum.Z);

        List<Document> results = newIndexAndQuery(docs, filter);

        assertThat(results.size(), is(3));
    }

    @Test
    public void testNotEqualsEnum() {

        List<Document> docs = List.of(
                createDoc(TestEnum.Y),
                createDoc(TestEnum.X),
                createDoc(TestEnum.X),
                createDoc(TestEnum.X),
                createDoc(TestEnum.X),
                createDoc(TestEnum.Y),
                createDoc(TestEnum.Z));

        LuceneFilter filter = new LuceneFilter();
        filter.addNotNull(ENUM_FIELD);
        filter.addNotEquals(ENUM_FIELD, TestEnum.Y);

        List<Document> results = newIndexAndQuery(docs, filter);

        assertThat(results.size(), is(5));
        for (Document document : results) {
            assertThat(document.get(ENUM_FIELD), not("Y"));
        }
    }

    @Test
    public void testContains() {
        List<Document> docs = List.of(
                createDoc("some test* string", 9),
                createDoc("shouldnt match TEST xyzz", 777),
                createDoc("FOOBAR TEST* BLARG", 9),
                createDoc("*****", 777),
                createDoc("lorem ipsum", 777));

        LuceneFilter filter = new LuceneFilter();
        filter.addContains(TEXT_FIELD, "test*");

        List<Document> results = newIndexAndQuery(docs, filter);

        assertThat(results.size(), is(2));
        for (Document document : results) {
            assertThat(getDouble(document), lessThan(10.0));
        }
    }

    @Test
    public void testNotContains() {
        List<Document> docs = List.of(
                createDoc("some test? string", 999),
                createDoc("shouldnt match TEST xyzz", 7),
                createDoc("FOOBAR TEST? BLARG", 999),
                createDoc("**??***", 7),
                createDoc("lorem ipsum", 7));

        LuceneFilter filter = new LuceneFilter();
        filter.addNotNull(TEXT_FIELD);
        filter.addNotContains(TEXT_FIELD, "test?");

        List<Document> results = newIndexAndQuery(docs, filter);

        assertThat(results.size(), is(3));
        for (Document document : results) {
            assertThat(getDouble(document), lessThan(10.0));
        }
    }

    @Test
    public void testStartsWith() {
        List<Document> docs = List.of(
                createDoc("the quick brown fox", 7),
                createDoc("the slow grey slug", 7),
                createDoc("FOOBAR TEST? BLARG", 999),
                createDoc("**??***", 999),
                createDoc("the lorem ipsum", 7));

        // Starts With
        {
            LuceneFilter filter = new LuceneFilter();
            filter.addStartsWith(TEXT_FIELD, "THE");

            List<Document> results = newIndexAndQuery(docs, filter);

            assertThat(results.size(), is(3));
            for (Document document : results) {
                assertThat(getDouble(document), lessThan(10.0));
            }
        }

        // NOT Starts With
        {
            LuceneFilter filter = new LuceneFilter();
            filter.addNotNull(TEXT_FIELD);
            filter.addNotStartsWith(TEXT_FIELD, "THE");

            List<Document> results = newIndexAndQuery(docs, filter);

            assertThat(results.size(), is(2));
            for (Document document : results) {
                assertThat(getDouble(document), greaterThan(10.0));
            }
        }
    }

    @Test
    public void testEndsWith() {
        List<Document> docs = List.of(
                createDoc("blue", 7),
                createDoc("purple", 7),
                createDoc("green", 999),
                createDoc("pink", 999),
                createDoc("ROSE", 7));

        // ends With
        {
            LuceneFilter filter = new LuceneFilter();
            filter.addEndsWith(TEXT_FIELD, "e");

            List<Document> results = newIndexAndQuery(docs, filter);

            assertThat(results.size(), is(3));
            for (Document document : results) {
                assertThat(getDouble(document), lessThan(10.0));
            }
        }

        // NOT ends With
        {
            LuceneFilter filter = new LuceneFilter();
            filter.addNotNull(TEXT_FIELD);
            filter.addNotEndsWith(TEXT_FIELD, "E");

            List<Document> results = newIndexAndQuery(docs, filter);

            assertThat(results.size(), is(2));
            for (Document document : results) {
                assertThat(getDouble(document), greaterThan(10.0));
            }
        }
    }

    @Test
    public void testStringRangeQueries() {

        List<Document> docs = List.of(
                createDoc("Aqua"),
                createDoc("Black"),
                createDoc("Blue"),
                createDoc("Fuchsia"),
                createDoc("Gray"),
                createDoc("Green"),
                createDoc("Lime"),
                createDoc("Maroon"),
                createDoc("Navy"),
                createDoc("Olive"),
                createDoc("Purple"),
                createDoc("Red"),
                createDoc("Silver"),
                createDoc("Teal"),
                createDoc("White"),
                createDoc("Yellow"));

        // Greater Than
        {
            LuceneFilter filter = new LuceneFilter();
            filter.addGreaterThan(TEXT_FIELD, "teal");

            List<Document> results = newIndexAndQuery(docs, filter);

            Set<String> colours = results.stream()
                                         .map(d -> d.get(TEXT_FIELD))
                                         .collect(toSet());

            assertThat(colours, is(Set.of("White", "Yellow")));
        }

        // Greater Than Or Equal To
        {
            LuceneFilter filter = new LuceneFilter();
            filter.addGreaterThanOrEqualTo(TEXT_FIELD, "teal");

            List<Document> results = newIndexAndQuery(docs, filter);

            Set<String> colours = results.stream()
                                         .map(d -> d.get(TEXT_FIELD))
                                         .collect(toSet());

            assertThat(colours, is(Set.of("Teal", "White", "Yellow")));
        }

        // Less than
        {
            LuceneFilter filter = new LuceneFilter();
            filter.addLessThan(TEXT_FIELD, "Black");

            List<Document> results = newIndexAndQuery(docs, filter);

            Set<String> colours = results.stream()
                                         .map(d -> d.get(TEXT_FIELD))
                                         .collect(toSet());

            assertThat(colours, is(Set.of("Aqua")));
        }

        // Less than or equal
        {
            LuceneFilter filter = new LuceneFilter();
            filter.addLessThanOrEqualTo(TEXT_FIELD, "Black");

            List<Document> results = newIndexAndQuery(docs, filter);

            Set<String> colours = results.stream()
                                         .map(d -> d.get(TEXT_FIELD))
                                         .collect(toSet());

            assertThat(colours, is(Set.of("Aqua", "Black")));
        }

        // Between
        {
            LuceneFilter filter = new LuceneFilter();
            filter.addBetween(TEXT_FIELD, "Black", "Green");

            List<Document> results = newIndexAndQuery(docs, filter);

            Set<String> colours = results.stream()
                                         .map(d -> d.get(TEXT_FIELD))
                                         .collect(toSet());

            assertThat(colours, is(Set.of("Black", "Blue", "Fuchsia", "Gray", "Green")));
        }

        // Between #2
        {
            LuceneFilter filter = new LuceneFilter();
            filter.addBetween(TEXT_FIELD, "Yoghut", "Zzzzzz");

            List<Document> results = newIndexAndQuery(docs, filter);
            assertThat(results, is(emptyList()));
        }

        // In
        {
            LuceneFilter filter = new LuceneFilter();
            filter.addIn(TEXT_FIELD, "Black", "foo", "bar", "baz");

            List<Document> results = newIndexAndQuery(docs, filter);

            Set<String> colours = results.stream()
                                         .map(d -> d.get(TEXT_FIELD))
                                         .collect(toSet());

            assertThat(colours, is(Set.of("Black")));
        }

        // Not In
        {
            LuceneFilter filter = new LuceneFilter();
            filter.addNotNull(TEXT_FIELD);
            filter.addNotIn(TEXT_FIELD, "Black", "foo", "bar", "baz");

            List<Document> results = newIndexAndQuery(docs, filter);

            Set<String> colours = results.stream()
                                         .map(d -> d.get(TEXT_FIELD))
                                         .collect(toSet());

            // Every color except 'black' from the list above
            assertThat(colours, is(Set.of("Aqua", "Blue", "Fuchsia", "Gray",
                    "Green", "Lime", "Maroon", "Navy", "Olive", "Purple",
                    "Red", "Silver", "Teal", "White", "Yellow")));
        }
    }

    @Test
    public void testDateRangeQueries() {

        final Instant start = Instant.parse("2014-01-01T08:00:00Z");
        List<Document> docs = datesSeries(start, Duration.ofDays(1)).limit(10)
                                                                    .map(d -> createDoc(d))
                                                                    .collect(toList());

        // This date appears in the 10 dates we just generated
        // so we can distunguish the results of the 'OrEqualTo' criteria
        Date queryDate = Date.from(Instant.parse("2014-01-08T08:00:00Z"));

        // Greater than
        {
            LuceneFilter filter = new LuceneFilter();
            filter.addGreaterThan(DATE_FIELD, queryDate);

            List<Document> results = newIndexAndQuery(docs, filter);
            assertThat(results.size(), is(2));
        }

        // Greater than or equal to
        {
            LuceneFilter filter = new LuceneFilter();
            filter.addGreaterThanOrEqualTo(DATE_FIELD, queryDate);

            List<Document> results = newIndexAndQuery(docs, filter);
            assertThat(results.size(), is(3));
        }

        // Less than
        {
            LuceneFilter filter = new LuceneFilter();
            filter.addLessThan(DATE_FIELD, queryDate);

            List<Document> results = newIndexAndQuery(docs, filter);
            assertThat(results.size(), is(7));
        }

        // Less than or equal to
        {
            LuceneFilter filter = new LuceneFilter();
            filter.addLessThanOrEqualTo(DATE_FIELD, queryDate);

            List<Document> results = newIndexAndQuery(docs, filter);
            assertThat(results.size(), is(8));
        }

        // Between
        {
            LuceneFilter filter = new LuceneFilter();
            filter.addBetween(DATE_FIELD, queryDate, queryDate);

            List<Document> results = newIndexAndQuery(docs, filter);
            assertThat(results.size(), is(1));
        }
    }

    private Stream<Date> datesSeries(Instant start, Duration step) {

        AtomicReference<Instant> ref = new AtomicReference<>(start);
        return Stream.generate(() -> {

            Instant instant = ref.get();
            Date d = Date.from(instant);

            instant = instant.plus(step);
            ref.set(instant);

            return d;
        })
                     .limit(100);
    }

    @Test
    public void testIntRangeQueries() {

        AtomicInteger i = new AtomicInteger(1);
        List<Document> docs = Stream.generate(() -> createDoc(i.getAndIncrement()))
                                    .limit(10)
                                    .collect(toList());

        int targetVal = 8;

        // GT
        {
            LuceneFilter filter = new LuceneFilter();
            filter.addGreaterThan(INT_FIELD, targetVal);

            List<Document> results = newIndexAndQuery(docs, filter);
            assertThat(results.size(), is(2));
        }

        // GTE
        {
            LuceneFilter filter = new LuceneFilter();
            filter.addGreaterThanOrEqualTo(INT_FIELD, targetVal);

            List<Document> results = newIndexAndQuery(docs, filter);
            assertThat(results.size(), is(3));
        }

        // LT
        {
            LuceneFilter filter = new LuceneFilter();
            filter.addLessThan(INT_FIELD, targetVal);

            List<Document> results = newIndexAndQuery(docs, filter);
            assertThat(results.size(), is(7));
        }

        // LTE
        {
            LuceneFilter filter = new LuceneFilter();
            filter.addLessThanOrEqualTo(INT_FIELD, targetVal);

            List<Document> results = newIndexAndQuery(docs, filter);
            assertThat(results.size(), is(8));
        }

        // Between
        {
            LuceneFilter filter = new LuceneFilter();
            filter.addBetween(INT_FIELD, 4, 7);

            List<Document> results = newIndexAndQuery(docs, filter);
            assertThat(results.size(), is(4));
        }
    }

    @Test
    public void testLongRangeQueries() {

        AtomicLong i = new AtomicLong(1);
        List<Document> docs = Stream.generate(() -> createDoc(i.getAndIncrement()))
                                    .limit(10)
                                    .collect(toList());

        long targetVal = 8;

        // GT
        {
            LuceneFilter filter = new LuceneFilter();
            filter.addGreaterThan(LONG_FIELD, targetVal);

            List<Document> results = newIndexAndQuery(docs, filter);
            assertThat(results.size(), is(2));
        }

        // GTE
        {
            LuceneFilter filter = new LuceneFilter();
            filter.addGreaterThanOrEqualTo(LONG_FIELD, targetVal);

            List<Document> results = newIndexAndQuery(docs, filter);
            assertThat(results.size(), is(3));
        }

        // LT
        {
            LuceneFilter filter = new LuceneFilter();
            filter.addLessThan(LONG_FIELD, targetVal);

            List<Document> results = newIndexAndQuery(docs, filter);
            assertThat(results.size(), is(7));
        }

        // LTE
        {
            LuceneFilter filter = new LuceneFilter();
            filter.addLessThanOrEqualTo(LONG_FIELD, targetVal);

            List<Document> results = newIndexAndQuery(docs, filter);
            assertThat(results.size(), is(8));
        }

        // Between
        {
            LuceneFilter filter = new LuceneFilter();
            filter.addBetween(LONG_FIELD, 4l, 7l);

            List<Document> results = newIndexAndQuery(docs, filter);
            assertThat(results.size(), is(4));
        }
    }

    @Test
    public void testDoubleRangeQueries() {

        AtomicDouble i = new AtomicDouble(1.0);
        List<Document> docs = Stream.generate(() -> {
            Document doc = new Document();
            doc.add(new DoubleField(DOUBLE_FIELD, i.getAndAdd(1.0), Store.YES));
            return doc;
        })
                                    .limit(10)
                                    .collect(toList());

        Decimal targetVal = new Decimal2("8.0");

        // GT
        {
            LuceneFilter filter = new LuceneFilter();
            filter.addGreaterThan(DOUBLE_FIELD, targetVal);

            List<Document> results = newIndexAndQuery(docs, filter);
            assertThat(results.size(), is(2));
        }

        // GTE
        {
            LuceneFilter filter = new LuceneFilter();
            filter.addGreaterThanOrEqualTo(DOUBLE_FIELD, targetVal);

            List<Document> results = newIndexAndQuery(docs, filter);
            assertThat(results.size(), is(3));
        }

        // LT
        {
            LuceneFilter filter = new LuceneFilter();
            filter.addLessThan(DOUBLE_FIELD, targetVal);

            List<Document> results = newIndexAndQuery(docs, filter);
            assertThat(results.size(), is(7));
        }

        // LTE
        {
            LuceneFilter filter = new LuceneFilter();
            filter.addLessThanOrEqualTo(DOUBLE_FIELD, targetVal);

            List<Document> results = newIndexAndQuery(docs, filter);
            assertThat(results.size(), is(8));
        }

        // Between
        {
            LuceneFilter filter = new LuceneFilter();
            filter.addBetween(DOUBLE_FIELD, new Decimal2(4.0), new Decimal2(7.0));

            List<Document> results = newIndexAndQuery(docs, filter);
            assertThat(results.size(), is(4));
        }
    }

    private int getInt(Document doc) {
        return doc.getField(INT_FIELD)
                  .numericValue()
                  .intValue();
    }

    private double getDouble(Document doc) {
        return doc.getField(DOUBLE_FIELD)
                  .numericValue()
                  .doubleValue();
    }

    private long getLong(Document doc) {
        return doc.getField(LONG_FIELD)
                  .numericValue()
                  .longValue();
    }

    /**
     * Create an index document with a string and double field
     * 
     * @param text
     * @param doub
     * @return
     */
    private Document createDoc(String text, double doub) {

        Document doc = new Document();

        doc.add(new TextField(TEXT_FIELD, text, Store.YES));
        doc.add(new DoubleField(DOUBLE_FIELD, doub, Store.YES));

        return doc;
    }

    private Document createDoc(String text) {

        Document doc = new Document();
        doc.add(new TextField(TEXT_FIELD, text, Store.YES));
        return doc;
    }

    /**
     * LuceneFilter expects booleans to be stored as strings: "true"/"false"
     * 
     * @param b
     * @return
     */
    private Document createDoc(boolean b) {
        Document doc = new Document();

        doc.add(new TextField(BOOLEAN_FIELD, String.valueOf(b), Store.YES));

        return doc;
    }

    private Document createDoc(Enum<?> e) {
        Document doc = new Document();

        doc.add(new TextField(ENUM_FIELD, String.valueOf(e), Store.YES));

        return doc;
    }

    /**
     * Create a doc containing the given Decimal, stored as a double value
     */
    private Document createDoc(Decimal val) {
        Document doc = new Document();
        doc.add(new DoubleField(DECIMAL_FIELD, val.doubleValue(), Store.YES));
        return doc;
    }

    private Document createDoc(Integer intValue) {

        Document doc = new Document();
        doc.add(new IntField(INT_FIELD, intValue, Store.YES));
        return doc;
    }

    private Document createDoc(Long intValue) {

        Document doc = new Document();
        doc.add(new LongField(LONG_FIELD, intValue, Store.YES));
        return doc;
    }

    private Document createDoc(Date date, double doub) {

        Document doc = new Document();

        doc.add(new DoubleField(DOUBLE_FIELD, doub, Store.YES));
        doc.add(new TextField(DATE_FIELD, dateToString(date), Store.YES));

        return doc;
    }

    private Document createDoc(Date date) {

        Document doc = new Document();
        doc.add(new TextField(DATE_FIELD, dateToString(date), Store.YES));
        return doc;
    }

    /**
     * This has to match whatever has been implemented in the document
     * converter (or whatever gets used to insert into the index).
     * 
     * @param date
     * @return
     */
    private static String dateToString(Date date) {
        return DateTools.dateToString(date, Resolution.MILLISECOND);
    }

    /**
     * Create a new in-memory index, insert the supplied Documents and run the given query
     * 
     * @param docsToInsert
     * @param filter
     * @return
     */
    private List<Document> newIndexAndQuery(Collection<Document> docsToInsert, LuceneFilter filter) {

        try (Directory dir = new ByteBuffersDirectory()) {

            // Write supplied documents into an in-memory index
            try (Analyzer analyzer = customAnalyzer()) {

                IndexWriterConfig iwc = new IndexWriterConfig(analyzer);
                iwc.setOpenMode(IndexWriterConfig.OpenMode.CREATE);

                try (IndexWriter writer = new IndexWriter(dir, iwc)) {

                    if (debugPrint) {
                        System.out.println("Inserting: ");
                        docsToInsert.forEach(d -> System.out.println("\t" + d));
                    }

                    for (Document doc : docsToInsert) {

                        writer.addDocument(doc);
                    }
                }
            }

            // Query that index
            try (IndexReader reader = DirectoryReader.open(dir)) {
                IndexSearcher searcher = new IndexSearcher(reader);

                Query query = filter.toQuery();

                TopDocs topDocs = searcher.search(query, MAX_RESULTS);

                StoredFields sf = reader.storedFields();
                List<Document> searchResults = Stream.of(topDocs.scoreDocs)
                                                     .map(sd -> {
                                                         try {
                                                             return sf.document(sd.doc);
                                                         } catch (IOException e) {
                                                             throw new RuntimeException(e);
                                                         }
                                                     })
                                                     .collect(toList());

                if (debugPrint) {
                    System.out.println("=================");
                    System.out.println("Query: \n\t" + query);
                    System.out.println("ScoreDocs:");
                    Stream.of(topDocs.scoreDocs)
                          .forEach(r -> System.out.println("\t" + r));
                    System.out.println("Results:");
                    searchResults.forEach(r -> System.out.println("\t" + r));
                    System.out.println("=================\n");
                }

                return searchResults;
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

    }

    private static Analyzer customAnalyzer() {

        try {
            return CustomAnalyzer.builder()
                                 .addTokenFilter(LowerCaseFilterFactory.NAME)
                                 .withTokenizer(KeywordTokenizerFactory.NAME)
                                 .build();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

    }
}
