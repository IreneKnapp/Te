PRAGMA fullfsync = 1;
PRAGMA foreign_keys = 1;
PRAGMA encoding = 'UTF-8';
CREATE TABLE settings (
  program TEXT,
  schema_version INTEGER,
  creation_timestamp INTEGER,
  modification_timestamp INTEGER
);
CREATE TABLE inodes (
  id BLOB PRIMARY KEY,
  name TEXT,
  cased_name TEXT,
  parent BLOB REFERENCES inodes(id)
              ON DELETE CASCADE
              ON UPDATE CASCADE,
  kind TEXT,
  size INTEGER,
  creation_timestamp INTEGER,
  modification_timestamp INTEGER,
  CONSTRAINT inode_name_unique_within_parent
  UNIQUE (parent, name),
  CONSTRAINT inode_id_valid
  CHECK ((typeof(id) = 'blob') AND (length(id) = 16)),
  CONSTRAINT inode_name_valid
  CHECK ((typeof(name) = 'text')
         AND (name NOT LIKE '%/%')),
  CONSTRAINT inode_cased_name_valid
  CHECK (name = upper(cased_name)),
  CONSTRAINT inode_kind_valid
  CHECK ((typeof(kind) = 'text') AND (kind != '')),
  CONSTRAINT inode_creation_timestamp_valid
  CHECK ((typeof(creation_timestamp) = 'integer')
         AND (creation_timestamp > 0)),
  CONSTRAINT inode_modification_timestamp_valid
  CHECK ((typeof(modification_timestamp) = 'integer')
         AND (modification_timestamp > 0)),
  CONSTRAINT inode_root_is_directory
  CHECK ((parent IS NOT NULL)
         OR ((kind = 'directory') AND (name = ''))),
  CONSTRAINT inode_sizes_where_appropriate
  CHECK (((kind = 'directory') AND (size IS NULL))
         OR ((kind != 'directory')
             AND (typeof(size) = 'integer')))
);
CREATE TABLE windows (
  id BLOB PRIMARY KEY,
  top INTEGER CHECK ((top IS NULL)
                     OR (typeof(top) = 'integer')),
  left INTEGER CHECK ((left IS NULL)
                      OR (typeof(left) = 'integer')),
  height INTEGER CHECK ((height IS NULL)
                        OR ((typeof(height) = 'integer')
                            AND (height > 0))),
  width INTEGER CHECK ((width IS NULL)
                       OR ((typeof(width) = 'integer')
                           AND (width > 0))),
  CONSTRAINT window_id_valid
  CHECK ((typeof(id) = 'blob') AND (length(id) = 16)),
  CONSTRAINT window_frames_all_or_none
  CHECK (((top IS NULL)
          AND (left IS NULL)
          AND (height IS NULL)
          AND (width IS NULL))
         OR
         ((top IS NOT NULL)
          AND (left IS NOT NULL)
          AND (height IS NOT NULL)
          AND (width IS NOT NULL)))
);
CREATE TABLE browser_windows (
  id BLOB PRIMARY KEY REFERENCES windows(id)
                      ON DELETE CASCADE
                      ON UPDATE CASCADE,
  root_inode BLOB REFERENCES inodes(id)
                  ON DELETE SET NULL
                  ON UPDATE CASCADE
);
CREATE TABLE browser_items (
  inode BLOB REFERENCES inodes(id)
             ON DELETE CASCADE
             ON UPDATE CASCADE,
  browser_window BLOB REFERENCES browser_windows(id)
                      ON DELETE CASCADE
                      ON UPDATE CASCADE,
  expanded INTEGER CHECK ((typeof(expanded) = 'integer')
                          AND ((expanded = 0)
                               OR (expanded = 1))),
  PRIMARY KEY (inode, browser_window)
);
INSERT INTO settings (program, schema_version,
creation_timestamp, modification_timestamp) VALUES
('com.dankna.te', 1, ?, ?);
INSERT INTO inodes (id, name, cased_name, parent,
kind, size, creation_timestamp, modification_timestamp)
VALUES (?, '', '', NULL, 'directory', NULL, ?, ?);
