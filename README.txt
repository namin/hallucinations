This project is about taking the Yip & Sussman learning ideas and
applying them to make generic sparse-stream learners that shape 
a module's I/O toward more acceptable forms.

For a test, we're going to do recognition of letters from a stream of
1D black & white pixel readings.  For example, this sequence of 14
inputs contains a very crude rendering of an A:

  0123456789...
  
8 ______11______...
7 _____1__1_____...
6 ____1____1____...
5 ___1______1___...
4 ___11111111___...
3 __1________1__...
2 __1________1__...
1 __1________1__...
0 __1________1__...

(showing '0' as '_' to increase pattern visibility)

To handle scaling problems, we want to incorporate something like
Allen and Borchardt relations.  So first, we'll reinterpret the
sequence of pixels as a set of intervals of black.  Thus, for
example, time slice 2 is {(0,3)} and time slice 4 is {(4,4),(6,6)}

We can then descibe a time pattern by a network of changes in the set
of intervals and their motion over time.  An edge in this network,
describing the evolution of an interval over time, means that all of
the intervals in that edge have been judged to refer to the "same"
object.  We will judge two intervals to refer to the same object if
they satisfy the following similarity relation: some pixel of the
interval at time T is the same as or adjacent to a pixel in an
interval at time T+1.

There are possible 4 operations that change the set of intervals:
- A interval can (A)ppear where there was no interval.
  [An interval at time T+1 has zero similar time T intervals]
- An existing interval can (D)isappear
  [An interval at time T has zero similar time T+1 intervals]
- A interval can (S)plit into two or more intervals.
  [An interval at time T has >1 similar time T+1 intervals]
- Two or more intervals can (M)erge into a single interval.
  [An interval at time T+1 has >1 similar time T intervals]

Note that a interval can participate in both a split and a merge at
the same time.  Consider, for example, the following sequence:

  T T+1
8 _ _
7 1 _
6 1 _
5 _ 1
4 _ 1
3 _ 1
2 1 _
1 1 1
0 _ 1

The interval (1,2) splits to intervals (0,1) and (3,5), yet (3,5) is a
merge of (1,2) and (6,7).  A brief review of some sample text,
however, turns up no cases of this ambiguity, so we will not worry
about it further until forced to confront it.  This is the only
ambiguity that can occur in interval set changes: all other pairings
are mutually exclusive.

At any instant, an existing interval can change in 9 ways, as its min
and max (r)ise, (f)all, (e)xpand, (c)ontract or (n)ot change.  We can
use these microchanges to segment the trajectory of a interval into
rising and falling sections.  Some transitions are ambiguous, which
means that they can be incorporated into either rising or falling
sections.  When an unambiguous (C)hange happens, though, we can
categorize the segment of trajectory leading up to the change.

Min Max | Trajectory
 r   r  |  r
 r   f  |  -
 r   n  |  r
 f   r  |  -
 f   f  |  f
 f   n  |  f
 n   r  |  r
 n   f  |  f
 n   n  |  -
Expand and Contract are done by measuring size

The upper interval of the 'A' sequence from time steps 4 to 9 thus has the
trajectory 'RR-FF', which is a rising segment, followed by a falling
segment, with a change occuring in the time range 6-7, where it is
ambiguous.  We can represent this either by picking an arbitrary
change point, by designating the time of the change to be an interval
rather than a point, or by allowing the two segments to overlap.

Thus, for example, the 14 input rendering of the A turns into the
following network of changes:

     /-r-C-f-\
A-r-S         M-f-D
     \-------/

reading time left to right, space bottom to top, and with edges
indicated by lines, changes in upper case, and trajectory segments in
lower case.
