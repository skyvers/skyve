package org.skyve.impl.archive.job;

import static java.util.stream.Collectors.joining;

import java.util.Optional;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.skyve.archive.support.DocumentConverter;

import jakarta.enterprise.inject.Instance;

/**
 * Finds the {@link DocumentConverter} for an archived document type, shared by the archive jobs.
 */
final class DocumentConverterLookup {
    private static final Logger logger = LogManager.getLogger();

    private DocumentConverterLookup() {
        // utility
    }

    /**
     * Find (via CDI) a DocumentConverter instance that can handle the requested module+document (as determined
     * by calling <em>DocumentConverter.handles(String, String)</em> on available instances.
     * <p>
     * If more than one DocumentConverter instance can handle the document type the particular instance
     * which is returned is undefined.
     *
     * @param documentConverters The injected converter instances.
     * @param module The module of the archived document type.
     * @param document The archived document type.
     * @return An optional containing a suitable DocumentConverter, or an empty option if no suitable converter is found.
     */
    static Optional<DocumentConverter> find(Instance<DocumentConverter> documentConverters, String module, String document) {
        Optional<DocumentConverter> converter = documentConverters.stream()
                                                                  .filter(dc -> dc.handles(module, document))
                                                                  .findFirst();

        if (converter.isPresent()) {
            logger.debug("Using converter: {} for {}.{}", converter, module, document);
        } else {
            logger.warn("No suitable document converter found for {}.{}; available converters: {}", module, document,
                    documentConverters.stream()
                                      .map(Object::toString)
                                      .collect(joining(", ")));
        }

        return converter;
    }
}
