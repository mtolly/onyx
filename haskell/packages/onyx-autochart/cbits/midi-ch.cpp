/*
This is a very hasty stripped-down port of logic from Edward Haas's autocharter:
https://github.com/EFHIII/midi-ch
*/

#include <vector>
#include <tuple>
#include <set>
#include <algorithm>
#include <cstdio>

std::tuple<int, int> midich_findInGroups(int note, std::vector<std::vector<int>> &groups) {
  for (int i = 0; i < groups.size(); i++) {
    for (int j = 0; j < groups[i].size(); j++) {
      if (groups[i][j] == note) {
        return std::make_tuple(i, j);
      }
    }
  }
  return std::make_tuple(-1, -1);
}

bool midich_mergable(std::vector<int> a, std::vector<int> b, std::vector<std::tuple<int, int>> &unChartedNotes, int frets) {
  std::set<int> distinct;
  for (int i = 0; i < a.size(); i++) {
    distinct.insert(std::get<1>(unChartedNotes[a[i]]));
  }
  for (int i = 0; i < b.size(); i++) {
    distinct.insert(std::get<1>(unChartedNotes[b[i]]));
  }
  if (distinct.size() > frets) {
    return false;
  }
  return true;
};

extern "C" {

// ticks and pitches should be arrays of "count" elements.
// notes should be sorted first by tick, then by pitch.
// on return, count may be modified, and ticks/pitches will hold the results.
void midich_compute(int frets, int *count, int *ticks, int *pitches) {

  std::vector<std::tuple<int, int>> unChartedNotes;

  int input_count = *count;

  if (input_count == 1) {
    // handle edge case, rest of logic loops due to empty "gaps" array
    pitches[0] = 0;
    return;
  }

  std::set<int> distinctNotes;
  for (int note = 0; note < input_count; note++) {
    distinctNotes.insert(pitches[note]);
  }

  for (int note = 0; note < input_count; note++) {
    unChartedNotes.push_back(std::make_tuple(
      ticks[note],
      std::distance(std::begin(distinctNotes), distinctNotes.find(pitches[note]))
    ));
  }

  std::vector<std::tuple<int, int>> gaps;
  for (int i = 1; i < unChartedNotes.size(); i++) {
    gaps.push_back(std::make_tuple(i, std::get<0>(unChartedNotes[i]) - std::get<0>(unChartedNotes[i - 1])));
  }

  std::sort(gaps.begin(), gaps.end(), [](const std::tuple<int, int> &a, const std::tuple<int, int> &b) -> bool {
    return std::get<1>(a) < std::get<1>(b);
  });

  std::vector<std::vector<int>> groups;

  for (int i = 0; i < gaps.size(); i++) {
    int index = std::get<0>(gaps[i]);
    int group1 = std::get<0>(midich_findInGroups(index - 1, groups));
    int group2 = std::get<0>(midich_findInGroups(index, groups));
    if (group2 >= 0) {
      if (group1 >= 0) {
        if (midich_mergable(groups[group1], groups[group2], unChartedNotes, frets)) {
          std::vector<int> concatted;
          for (int x : groups[group1]) {
            concatted.push_back(x);
          }
          for (int x : groups[group2]) {
            concatted.push_back(x);
          }
          groups[group1] = concatted;
          groups.erase(groups.begin() + group2);
        }
      } else {
        if (midich_mergable(groups[group2], {index - 1}, unChartedNotes, frets)) {
          groups[group2].push_back(index - 1);
        } else {
          groups.push_back({index - 1});
        }
      }
    } else if (group1 >= 0) {
      if(midich_mergable(groups[group1], {index}, unChartedNotes, frets)) {
        groups[group1].push_back(index);
      } else {
        groups.push_back({index});
      }
    } else {
      groups.push_back({index - 1, index});
    }
  }

  std::vector<std::set<int>> distinct;

  for (int g = 0; g < groups.size(); g++) {
    distinct.push_back({});
    for (int i = 0; i < groups[g].size(); i++) {
      distinct[g].insert(std::get<1>(unChartedNotes[groups[g][i]]));
    }
  }

  // if less than frets note range group, shift
  for (int g = 0; g < groups.size(); g++) {
    int d = 0;
    int gd = 1;
    while (distinct[g].size() < frets) {
      if (g - gd > 0 && d < groups[g - gd].size()) {
        distinct[g].insert(
          std::get<1>(unChartedNotes[groups[g - gd][groups[g - gd].size() - 1 - d]])
        );
      }

      if (g + gd < groups.size() - 1 && distinct[g].size() < frets) {
        if (d < groups[g + gd].size()) {
          distinct[g].insert(
            std::get<1>(unChartedNotes[groups[g + gd][d]])
          );
        }
      }
      d++;
      if ((g - gd < 0 || d >= groups[g - gd].size()) && (g + gd >= groups.size() - 2 || d >= groups[g + gd].size())) {
        gd++;
        d = 0;
      }
      if (g - gd < 0 && g + gd >= groups.size()) {
        // in js this added Infinity, and didn't remove dupes until out of the loop.
        // so instead we just come up with unique big numbers each time
        int bigNumber = 9999;
        if (!distinct[g].empty()) {
          bigNumber += *(distinct[g].end());
        }
        distinct[g].insert(bigNumber);
      }
    }
  }

  std::vector<std::tuple<int, int>> final_results;
  for (int i = 0; i < unChartedNotes.size(); i++) {
    std::tuple<int, int> group = midich_findInGroups(i, groups);
    auto haystack = distinct[std::get<0>(group)];
    auto needle = std::get<1>(unChartedNotes[groups[std::get<0>(group)][std::get<1>(group)]]);
    auto indexOf = std::find(haystack.begin(), haystack.end(), needle);
    if (indexOf != haystack.end()) {
      int index = std::distance(haystack.begin(), indexOf);
      final_results.push_back(std::make_tuple(std::get<0>(unChartedNotes[i]), index));
    }
  }

  *count = final_results.size();
  for (int i = 0; i < final_results.size(); ++i)
  {
    ticks[i] = std::get<0>(final_results[i]);
    pitches[i] = std::get<1>(final_results[i]);
  }
}

}
