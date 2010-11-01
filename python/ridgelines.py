class RidgeLineExtraction(RidgeExtraction):

    # Find connected ridge lines
    def ridge_lines(self):

        segments = self.ridge_segments_cached()
        lines = []

        # Note that this whole algorithm is evil, because it assumes
        # that the ridge_segments_cached() method ensures that only
        # one RidgePointInfo instance is created for each ridge point
        # in the image.

        for s in segments:
            # First line found which segment can fit onto
            first_line = None
            # True if s is at the end of first_line, otherwise False.
            at_end = True

            for l in lines[:]:
                # First, find any line that s fits onto.
                if (first_line == None):
                    if s.end == l[0].start:
                        l.appendleft(s)
                        first_line = l
                        at_end = False
                    elif s.start == l[0].start:
                        l.appendleft(s.reverse())
                        first_line = l
                        at_end = False
                    elif s.end == l[-1].end:
                        l.append(s.reverse())
                        first_line = l
                    elif s.start == l[-1].end:
                        l.append(s)
                        first_line = l

                # Otherwise, find out if there's another line that
                # after the addition of s can be merged ont first_line
                else:
                    merged = False
                    if at_end:
                        if s.end == l[0].start:
                            first_line.extend(l)
                            merged = True
                        elif s.end == l[-1].end:
                            l.reverse()
                            first_line.extend([x.reverse() for x in l])
                            merged = True
                    else:
                        if s.start == l[0].start:
                            first_line.extendleft([x.reverse() for x in l])
                            merged = True
                        elif s.start == l[-1].end:
                            l.reverse()
                            first_line.extendleft(l)
                            merged = True

                    if merged:
                        lines.remove(l)
                        print "Merged two lines"
                        break

            if first_line == None:
                lines.append(deque([s]))
                print "Added new line"
